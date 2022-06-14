// Copyright (c) 2022 SquintLang.
// Licensed under the Apache License, Version 2.0.
// Source repository: https://github.com/LanguageDev/SquintLang

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Squint.Compiler;

public enum SymbolKind
{
    Func,
    Type,
}

public sealed record class Symbol(string Name, SymbolKind Kind);

public sealed record class Scope(
    Scope? Parent,
    IDictionary<string, Symbol> Symbols)
{
    public bool IsGlobal => this.Parent is null;

    public void Define(Symbol sym)
    {
        if (!this.Symbols.TryGetValue(sym.Name, out var existing))
        {
            this.Symbols.Add(sym.Name, sym);
            return;
        }

        if (sym.Kind != existing.Kind) throw new InvalidOperationException();
    }

    public Symbol Reference(string name)
    {
        if (this.Symbols.TryGetValue(name, out var existing)) return existing;
        if (this.Parent is not null) return this.Parent.Reference(name);
        throw new InvalidOperationException();
    }
}

public static class SymbolResolution
{
    public static void Resolve(Ast ast)
    {
        new Pass1().Pass(ast);
        new Pass2().Pass(ast);
        new Pass3().Pass(ast);
    }

    // Define scope and order-independent symbols
    private sealed class Pass1 : AstVisitor<object>
    {
        private static Scope MakeScope(Scope? parent = null) => new(parent, new Dictionary<string, Symbol>());

        private readonly Scope globalScope;
        private Scope currentScope;

        public Pass1()
        {
            this.globalScope = MakeScope();
            this.currentScope = this.globalScope;

            void DefineBuiltinType(string name) => this.globalScope.Define(new(name, SymbolKind.Type));

            DefineBuiltinType("string");
            DefineBuiltinType("int");
        }

        private void PushScope() => this.currentScope = MakeScope(this.currentScope);
        private void PopScope() => this.currentScope = this.currentScope.Parent
                                                    ?? throw new InvalidOperationException();

        public void Pass(Ast ast) => this.Visit(ast);

        protected override object Visit(Stmt stmt)
        {
            stmt.Scope = this.currentScope;
            return base.Visit(stmt);
        }

        protected override object Visit(Decl decl)
        {
            decl.Scope = this.currentScope;
            return base.Visit(decl);
        }

        protected override object Visit(Expr expr)
        {
            expr.Scope = this.currentScope;
            return base.Visit(expr);
        }

        protected override object Visit(Pattern pattern)
        {
            pattern.Scope = this.currentScope;
            return base.Visit(pattern);
        }

        protected override object Visit(Decl.Func func)
        {
            this.PushScope();
            base.Visit(func);
            this.PopScope();
            return this.Default;
        }

        protected override object Visit(Stmt.Block block)
        {
            this.PushScope();
            base.Visit(block);
            this.PopScope();
            return this.Default;
        }

        protected override object Visit(Decl.Record record)
        {
            record.Symbol = new(record.Name, SymbolKind.Type);
            this.currentScope.Define(record.Symbol);

            this.PushScope();
            base.Visit(record);
            this.PopScope();
            return this.Default;
        }

        protected override object Visit(Decl.FuncSignature funcSignature)
        {
            base.Visit(funcSignature);
            funcSignature.Symbol = new(funcSignature.Name, SymbolKind.Func);
            funcSignature.Scope!.Define(funcSignature.Symbol);
            return this.Default;
        }
    }

    // Import resolution
    private sealed class Pass2 : AstVisitor<object>
    {
        public void Pass(Ast ast) => this.Visit(ast);

        protected override object Visit(Decl.Import import)
        {
            // TODO
            return this.Default;
        }
    }

    // Define order-dependent symbols and resolve all symbols
    private sealed class Pass3 : AstVisitor<object>
    {
        public void Pass(Ast ast) => this.Visit(ast);

        protected override object Visit(Expr.Name name)
        {
            name.Symbol = name.Scope!.Reference(name.Value);
            return this.Default;
        }
    }
}