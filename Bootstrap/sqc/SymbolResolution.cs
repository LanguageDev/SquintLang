// Copyright (c) 2022 SquintLang.
// Licensed under the Apache License, Version 2.0.
// Source repository: https://github.com/LanguageDev/SquintLang

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Squint.Compiler;

public enum SymbolKind { }

public sealed record class Symbol(string Name, SymbolKind Kind);

public sealed record class Scope(
    Scope? Parent,
    IDictionary<string, Symbol> Symbols);

public static class SymbolResolution
{
    public static void Resolve(Ast ast)
    {
        new Pass1().Pass(ast);
        new Pass2().Pass(ast);
    }

    private sealed class Pass1 : AstVisitor<object>
    {
        public void Pass(Ast ast) => this.Visit(ast);
    }

    private sealed class Pass2 : AstVisitor<object>
    {
        public void Pass(Ast ast) => this.Visit(ast);
    }
}
