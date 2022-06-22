using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.Json;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Basic.Reference.Assemblies;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Squint.Compiler.Syntax;

namespace Squint.Compiler;

public class CompilerArgs
{
    public bool DumpCSharp { get; set; }
    public bool Run { get; set; }
    public string OutputName { get; set; } = "out";
    public IList<string> SourceFiles { get; set; } = new List<string>();

    public static CompilerArgs Parse(string[] args)
    {
        var result = new CompilerArgs();
        for (var i = 0; i < args.Length; i++)
        {
            var arg = args[i];
            if (!arg.StartsWith("-"))
            {
                // Assumed to be an input file
                result.SourceFiles.Add(arg);
                continue;
            }
            // Some kind of flag
            if (arg == "--run") result.Run = true;
            else if (arg == "--dump-cs") result.DumpCSharp = true;
            else if (arg == "--out") result.OutputName = args[++i];
            else throw new InvalidOperationException();
        }
        if (result.SourceFiles.Count == 0) throw new InvalidOperationException();
        return result;
    }
}

internal class Program
{
    private static string GenerateRuntimeConfig() => $@"
{{
  ""runtimeOptions"": {{
    ""tfm"": ""net6.0"",
    ""framework"": {{
      ""name"": ""Microsoft.NETCore.App"",
      ""version"": ""6.0.0""
    }}
  }}
}}
";

    private static bool CompileCSharp(
        string code,
        string outputPath)
    {
        var outputName = Path.GetFileNameWithoutExtension(outputPath);
        var syntaxTree = CSharpSyntaxTree.ParseText(code);
        var compilation = CSharpCompilation.Create(
            assemblyName: outputName,
            syntaxTrees: new[] { syntaxTree },
            references: ReferenceAssemblies.Net60,
            options: new(
                outputKind: OutputKind.ConsoleApplication,
                mainTypeName: "Program"));
        var outputStream = new FileStream($"{outputPath}.dll", FileMode.Create);
        var emitResult = compilation.Emit(outputStream);
        outputStream.Close();
        if (emitResult.Success)
        {
            File.WriteAllText(
                Path.Combine(Path.GetDirectoryName(outputPath)!, $"{outputName}.runtimeconfig.json"),
                GenerateRuntimeConfig());
            return true;
        }

        Console.WriteLine("C# compiler failed!");
        foreach (var diag in emitResult.Diagnostics) Console.WriteLine(diag);
        return false;
    }

    private static T LoadFunc<T>(Assembly assembly, string name)
        where T : Delegate
    {
        var globals = assembly.GetType("Globals")!;
        var methodInfo = globals.GetMethod(name)!;
        return (T)Delegate.CreateDelegate(typeof(T), methodInfo);
    }

    private static void Execute(string name)
    {
        var startInfo = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"exec {name}.dll",
        };
        var process = Process.Start(startInfo) ?? throw new InvalidOperationException();
        process.WaitForExit();
        Console.WriteLine($"Process terminated with exit code: {process.ExitCode}");
    }

    private static void Main(string[] args)
    {
        var compilerArgs = CompilerArgs.Parse(args);

        try
        {
            // Combine syntax trees
            var asts = compilerArgs.SourceFiles
                .Select(f => (Decl)Ast.Parse(File.ReadAllText(f)))
                .ToImmutableList();
            var ast = new Decl.Seq(asts);
            var symbolTable = new SymbolTable();
            SymbolResolution.Resolve(ast, symbolTable);
            var code = Codegen.Generate(ast);

            if (compilerArgs.DumpCSharp)
            {
                Console.WriteLine("// GENERATED C# CODE ///////////////////////////////////////////////////////////");
                Console.WriteLine(code);
                Console.WriteLine("////////////////////////////////////////////////////////////////////////////////");
            }

            if (!CompileCSharp(code, compilerArgs.OutputName))
            {
                Environment.Exit(1);
                return;
            }

            if (compilerArgs.Run) Execute(compilerArgs.OutputName);
        }
        catch (CompilerError err)
        {
            Console.WriteLine("Compiler error:");
            Console.WriteLine(err.Message);
        }
    }
}
