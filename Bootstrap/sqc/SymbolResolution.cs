// Copyright (c) 2022 SquintLang.
// Licensed under the Apache License, Version 2.0.
// Source repository: https://github.com/LanguageDev/SquintLang

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace Squint.Compiler;

public enum SymbolKind
{
    Func,
    Type,
    Local,
    Global,
    Namespace,
    Label,
}

public enum ScopeKind
{
    Global,
    Type,
    Local,
    Function,
    Loop,
}

public sealed record class Symbol(string Name, SymbolKind Kind)
{
    public object UniqueKey { get; set; } = new();
    public string FullName { get; init; } = Name;
    public bool IsArgless { get; init; } = false;
    public Symbol? Supertype { get; set; }
}

public sealed record class Scope(
    Scope? Parent,
    IDictionary<string, Symbol> Symbols,
    ScopeKind Kind)
{
    public bool IsGlobal => this.Parent is null;

    public Symbol? BreakSymbol { get; set; }
    public Symbol? ContinueSymbol { get; set; }

    public void Define(Symbol sym, string? nameOverride = null)
    {
        var name = nameOverride ?? sym.Name;
        if (!this.Symbols.TryGetValue(name, out var existing))
        {
            this.Symbols.Add(name, sym);
            return;
        }

        if (sym.Kind != existing.Kind) throw new InvalidOperationException();
        // Override anyway
        this.Symbols[name] = sym;
    }

    public Symbol? ReferenceOpt(string name)
    {
        if (this.Symbols.TryGetValue(name, out var existing)) return existing;
        if (this.Parent is not null) return this.Parent.ReferenceOpt(name);
        return null;
    }

    public Symbol Reference(string name)
    {
        var result = this.ReferenceOpt(name);
        if (result is null) CompilerError.Report($"Undefined symbol '{name}'");
        return result;
    }
}

public sealed class SymbolTable
{
    private static Scope MakeScope(ScopeKind kind, Scope? parent = null) =>
        new(parent, new Dictionary<string, Symbol>(), kind);

    public Scope GlobalScope { get; }
    public Scope CurrentScope { get; private set; }

    public SymbolTable()
    {
        this.GlobalScope = MakeScope(ScopeKind.Global);
        this.CurrentScope = this.GlobalScope;

        void DefineBuiltinType(string name, string? realName = null) =>
            this.GlobalScope.Define(new(name, SymbolKind.Type)
            {
                FullName = realName ?? name,
            });

        DefineBuiltinType("object");
        DefineBuiltinType("string");
        DefineBuiltinType("char");
        DefineBuiltinType("int");
        DefineBuiltinType("bool");
        DefineBuiltinType("unit", "void");

        DefineBuiltinType("int8", "sbyte");
        DefineBuiltinType("int16", "short");
        DefineBuiltinType("int32", "int");
        DefineBuiltinType("int64", "long");

        DefineBuiltinType("uint8", "byte");
        DefineBuiltinType("uint16", "ushort");
        DefineBuiltinType("uint32", "uint");
        DefineBuiltinType("uint64", "ulong");

        // HACK: We register some namespaces
        this.GlobalScope.Define(new("System", SymbolKind.Namespace));
    }

    public void PushScope(ScopeKind kind) => this.CurrentScope = MakeScope(kind, this.CurrentScope);
    public void PopScope() => this.CurrentScope = this.CurrentScope.Parent
                                                ?? throw new InvalidOperationException();
}

public static class SymbolResolution
{
    public static void Resolve(Ast ast, SymbolTable symbolTable)
    {
        new Pass1(symbolTable).Pass(ast);
        new Pass2().Pass(ast);
        new Pass3().Pass(ast);
    }

    // Define scope and order-independent symbols
    private sealed class Pass1 : AstVisitor<object>
    {
        private readonly SymbolTable symbolTable;

        private Scope CurrentScope => this.symbolTable.CurrentScope;

        private void PushScope(ScopeKind scopeKind) => this.symbolTable.PushScope(scopeKind);
        private void PopScope() => this.symbolTable.PopScope();

        public Pass1(SymbolTable symbolTable)
        {
            this.symbolTable = symbolTable;
        }

        public void Pass(Ast ast) => this.Visit(ast);

        protected override object Visit(Stmt stmt)
        {
            stmt.Scope = this.CurrentScope;
            return base.Visit(stmt);
        }

        protected override object Visit(Decl decl)
        {
            decl.Scope = this.CurrentScope;
            return base.Visit(decl);
        }

        protected override object Visit(Expr expr)
        {
            expr.Scope = this.CurrentScope;
            return base.Visit(expr);
        }

        protected override object Visit(Pattern pattern)
        {
            pattern.Scope = this.CurrentScope;
            return base.Visit(pattern);
        }

        protected override object Visit(Decl.Func func)
        {
            func.Signature.Symbol = new(func.Signature.Name, SymbolKind.Func);
            this.CurrentScope.Define(func.Signature.Symbol);

            this.PushScope(ScopeKind.Function);
            base.Visit(func);
            this.PopScope();
            return this.Default;
        }

        protected override object Visit(Decl.Record record)
        {
            record.Symbol = new(record.Name, SymbolKind.Type);
            this.CurrentScope.Define(record.Symbol);

            this.PushScope(ScopeKind.Type);
            base.Visit(record);
            this.PopScope();
            return this.Default;
        }

        protected override object Visit(Decl.Enum @enum)
        {
            @enum.Symbol = new(@enum.Name, SymbolKind.Type);
            this.CurrentScope.Define(@enum.Symbol);

            this.PushScope(ScopeKind.Type);
            base.Visit(@enum);
            this.PopScope();

            // Propagate up variant symbols with the full name
            foreach (var v in @enum.Variants) this.CurrentScope.Define(v.Symbol!, v.Symbol!.FullName);

            return this.Default;
        }

        protected override object Visit(Decl.EnumVariant enumVariant)
        {
            enumVariant.Symbol = new(enumVariant.Name, SymbolKind.Type)
            {
                FullName = $"{enumVariant.Parent!.Symbol!.FullName}.{enumVariant.Name}",
                Supertype = enumVariant.Parent!.Symbol,
                IsArgless = enumVariant.Members is null,
            };
            this.CurrentScope.Define(enumVariant.Symbol);

            this.PushScope(ScopeKind.Type);
            base.Visit(enumVariant);
            this.PopScope();
            return this.Default;
        }

        protected override object Visit(Decl.Trait trait)
        {
            trait.Symbol = new(trait.Name, SymbolKind.Type);
            this.CurrentScope.Define(trait.Symbol);

            this.PushScope(ScopeKind.Type);

            // This is also a type refering to the implementor type
            var thisSymbol = new Symbol("This", SymbolKind.Type);
            this.CurrentScope.Define(thisSymbol);

            base.Visit(trait);
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

        protected override object Visit(Decl.Var var)
        {
            base.Visit(var);
            if (var.Scope!.IsGlobal)
            {
                var.Symbol = new(var.Name, SymbolKind.Global);
                var.Scope!.Define(var.Symbol);
            }
            return this.Default;
        }

        protected override object Visit(Stmt.Label label)
        {
            label.Symbol = new(label.Name, SymbolKind.Label);
            label.Scope!.Define(label.Symbol);
            return this.Default;
        }

        protected override object Visit(Expr.Block block)
        {
            this.PushScope(ScopeKind.Local);
            base.Visit(block);
            this.PopScope();
            return this.Default;
        }

        protected override object Visit(Expr.While @while)
        {
            this.PushScope(ScopeKind.Loop);
            // NOTE: We don't register them, lookup is based on scope
            this.CurrentScope.BreakSymbol = new("", SymbolKind.Label);
            this.CurrentScope.ContinueSymbol = new("", SymbolKind.Label);
            base.Visit(@while);
            this.PopScope();
            return this.Default;
        }

        protected override object Visit(Expr.For @for)
        {
            this.PushScope(ScopeKind.Loop);
            // NOTE: We don't register them, lookup is based on scope
            this.CurrentScope.BreakSymbol = new("", SymbolKind.Label);
            this.CurrentScope.ContinueSymbol = new("", SymbolKind.Label);
            base.Visit(@for);
            this.PopScope();
            return this.Default;
        }

        protected override object Visit(Expr.Match match)
        {
            this.Visit(match.Value);
            // Each arm gets a scope
            foreach (var arm in match.Arms)
            {
                this.PushScope(ScopeKind.Local);
                this.Visit(arm);
                this.PopScope();
            }
            return this.Default;
        }
    }

    // Import resolution
    private sealed class Pass2 : AstVisitor<object>
    {
        public void Pass(Ast ast) => this.Visit(ast);

        protected override object Visit(Decl.Import import)
        {
            var item = Import(import.Parts, import.Generics.Count);
            if (item is null) throw new InvalidOperationException();

            var symbol = null as Symbol;
            var fullName = string.Join('.', import.Parts);
            if (item is MethodInfo)
            {
                symbol = new(import.Parts[^1], SymbolKind.Func)
                {
                    FullName = fullName,
                };
            }
            else if (item is Type)
            {
                symbol = new(import.Parts[^1], SymbolKind.Type)
                {
                    FullName = fullName,
                };
            }
            else
            {
                var gen = import.Generics.Count == 0 ? "" : $"[{string.Join(", ", import.Generics)}]";
                CompilerError.Report($"Can't resolve import {string.Join(".", import.Parts)}{gen}");
            }

            import.Scope!.Define(symbol);

            return this.Default;
        }

        private static MemberInfo? Import(ImmutableList<string> parts, int argc)
        {
            // First try as a type
            var typePath = string.Join('.', parts);
            if (argc > 0) typePath = $"{typePath}`{argc}";
            foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
            {
                var ty = assembly.GetType(typePath);
                if (ty is not null) return ty;
            }

            // Try the first part as a type, second as a static function
            if (parts.Count > 1 && argc == 0)
            {
                typePath = string.Join('.', parts.SkipLast(1));
                foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
                {
                    var ty = assembly.GetType(typePath);
                    if (ty is null) continue;

                    var method = ty
                        .GetMethods(BindingFlags.Public | BindingFlags.Static)
                        .Where(m => m.Name == parts[^1])
                        .FirstOrDefault();
                    if (method is not null) return method;
                }
            }

            // Not found
            return null;
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

        protected override object Visit(Decl.Func func)
        {
            foreach (var p in func.Signature.Params)
            {
                this.Visit(p);
                p.Symbol = new(p.Name, SymbolKind.Local);
                func.Body.Scope!.Define(p.Symbol);
            }
            if (func.Signature.Ret is not null) this.Visit(func.Signature.Ret);
            this.Visit(func.Body);
            return this.Default;
        }

        protected override object Visit(Decl.Var var)
        {
            base.Visit(var);
            if (!var.Scope!.IsGlobal)
            {
                // Define local now
                var.Symbol = new(var.Name, SymbolKind.Local);
                var.Scope!.Define(var.Symbol);
            }
            return this.Default;
        }

        private static Scope? LookUpLoop(Scope scope)
        {
            if (scope.Kind == ScopeKind.Loop) return scope;
            if (scope.Parent is null) return null;
            return LookUpLoop(scope.Parent);
        }

        protected override object Visit(Stmt.Goto @goto)
        {
            @goto.Symbol = @goto.Name switch
            {
                "break" => LookUpLoop(@goto.Scope!)?.BreakSymbol ?? throw new InvalidOperationException(),
                "continue" => LookUpLoop(@goto.Scope!)?.ContinueSymbol ?? throw new InvalidOperationException(),
                _ => @goto.Scope!.Reference(@goto.Name),
            };
            return this.Default;
        }

        protected override object Visit(Expr.For @for)
        {
            this.Visit(@for.Iterated);
            @for.IteratorSymbol = new(@for.Iterator, SymbolKind.Local);
            @for.Body.Scope!.Define(@for.IteratorSymbol);
            this.Visit(@for.Body);
            return this.Default;
        }

        protected override object Visit(Pattern.Name name)
        {
            base.Visit(name);
            // Define local
            name.Symbol = new(name.Value, SymbolKind.Local);
            name.Scope!.Define(name.Symbol);
            return this.Default;
        }

        protected override object Visit(Pattern.Destructure destructure)
        {
            base.Visit(destructure);
            if (destructure.BoundName is not null)
            {
                // Bound to a name, register it
                destructure.BoundSymbol = new(destructure.BoundName, SymbolKind.Local);
                destructure.Scope!.Define(destructure.BoundSymbol);
            }
            return this.Default;
        }
    }
}
