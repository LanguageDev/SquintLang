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
}

public sealed record class Symbol(string Name, SymbolKind Kind)
{
    public string FullName { get; init; } = Name;
    public Symbol? Supertype { get; set; }
}

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

    public Symbol? ReferenceOpt(string name)
    {
        if (this.Symbols.TryGetValue(name, out var existing)) return existing;
        if (this.Parent is not null) return this.Parent.ReferenceOpt(name);
        return null;
    }

    public Symbol Reference(string name) =>
        this.ReferenceOpt(name) ?? throw new InvalidOperationException();
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

            void DefineBuiltinType(string name, string? realName = null) =>
                this.globalScope.Define(new(name, SymbolKind.Type)
                {
                    FullName = realName ?? name,
                });

            DefineBuiltinType("string");
            DefineBuiltinType("int");
            DefineBuiltinType("bool");
            DefineBuiltinType("unit", "void");

            // HACK: We register some namespaces
            this.globalScope.Define(new("System", SymbolKind.Namespace));
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
            func.Signature.Symbol = new(func.Signature.Name, SymbolKind.Func);
            this.currentScope.Define(func.Signature.Symbol);

            this.PushScope();
            base.Visit(func);
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

        protected override object Visit(Decl.Enum @enum)
        {
            @enum.Symbol = new(@enum.Name, SymbolKind.Type);
            this.currentScope.Define(@enum.Symbol);

            foreach (var v in @enum.Variants) v.Parent = @enum;

            this.PushScope();
            base.Visit(@enum);
            this.PopScope();

            // Propagate up variant symbols with the full name
            foreach (var v in @enum.Variants) this.currentScope.Define(new(v.Symbol!.FullName, v.Symbol!.Kind));

            return this.Default;
        }

        protected override object Visit(Decl.EnumVariant enumVariant)
        {
            enumVariant.Symbol = new(enumVariant.Name, SymbolKind.Type)
            {
                FullName = $"{enumVariant.Parent!.Symbol!.FullName}.{enumVariant.Name}",
                Supertype = enumVariant.Parent!.Symbol,
            };
            this.currentScope.Define(enumVariant.Symbol);

            this.PushScope();
            base.Visit(enumVariant);
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

        protected override object Visit(Expr.Block block)
        {
            this.PushScope();
            base.Visit(block);
            this.PopScope();
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
                throw new InvalidOperationException();
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
    }
}
