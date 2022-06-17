using System;
using System.IO;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Squint.Compiler.Syntax;

namespace Squint.Compiler;

internal class Program
{
    private static IParseTree ParseSyntaxTree(string text)
    {
        var inputStream = new AntlrInputStream(text);
        var lexer = new SquintLexer(inputStream);
        var commonTokenStream = new CommonTokenStream(lexer);
        var parser = new SquintParser(commonTokenStream);
        return parser.file();
    }

    private static void Main(string[] args)
    {
        var text = File.ReadAllText("test.squint");
        var ast = Ast.Parse(text);
        SymbolResolution.Resolve(ast);
        var code = Codegen.Generate(ast);
        Console.WriteLine(code);
    }
}
