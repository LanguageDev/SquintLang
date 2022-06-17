using System;
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

    private static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.WriteLine("Usage: sqc <file> [options]");
            Environment.Exit(1);
            return;
        }

        var text = File.ReadAllText(args[0]);
        var output = "out";
        var ast = Ast.Parse(text);
        SymbolResolution.Resolve(ast);
        var code = Codegen.Generate(ast);

        if (args.Contains("--dump-cs"))
        {
            Console.WriteLine("// GENERATED C# CODE ///////////////////////////////////////////////////////////");
            Console.WriteLine(code);
            Console.WriteLine("////////////////////////////////////////////////////////////////////////////////");
        }

        if (args.Contains("--out"))
        {
            var idx = Array.IndexOf(args, "--out");
            output = args[idx + 1];
        }

        if (!CompileCSharp(code, output))
        {
            Environment.Exit(1);
            return;
        }
    }
}
