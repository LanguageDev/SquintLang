// Copyright (c) 2022 SquintLang.
// Licensed under the Apache License, Version 2.0.
// Source repository: https://github.com/LanguageDev/SquintLang

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace Squint.Compiler;

public sealed class Codegen : AstVisitor<string>
{
    public static string Generate(Ast ast)
    {
        var gen = new Codegen();
        gen.PushContext(gen.globalsBuilder);
        gen.Visit(ast);
        gen.PopContext();
        return gen.Code;
    }

    public sealed class TypeBuilder
    {
        public string Kind { get; set; } = "class";
        public bool Abstract { get; set; } = false;
        public bool Open { get; set; } = false;
        public string Name { get; set; } = "Unnamed";
        public HashSet<string> Bases { get; set; } = new();
        public StringBuilder CodeBuilder { get; set; } = new();
        public Dictionary<Symbol, TypeBuilder> SubtypeBuilders { get; set; } = new();

        public string Code
        {
            get
            {
                var result = new StringBuilder();
                result.Append("public ");
                if (this.Abstract) result.Append("abstract ");
                if (!this.Open && !this.Abstract) result.Append("sealed ");
                result.Append(this.Kind).Append(' ');
                result.Append(this.Name);
                if (this.Bases.Count > 0) result.Append(" : ").AppendJoin(", ", this.Bases);
                result.AppendLine();
                result.AppendLine("{");
                foreach (var sub in this.SubtypeBuilders.Values) result.AppendLine(sub.Code);
                result.AppendLine(this.CodeBuilder.ToString().TrimEnd());
                result.AppendLine("}");
                return result.ToString();
            }
        }
    }

    public string Code => SyntaxFactory.ParseCompilationUnit(this.CodeRaw)
        .NormalizeWhitespace()
        .GetText()
        .ToString();

    public string CodeRaw => $@"
public readonly struct Unit {{ }}
public static class Prelude
{{
    public static T DeVoid<T>(System.Func<T> f) => f();
    public static Unit DeVoid(System.Action f)
    {{
        f();
        return default(Unit);
    }}
}}

public static class Globals
{{
    {this.globalsBuilder}
}}

{string.Join("\n", this.types.Values.Select(b => b.Code))}
";

    private readonly Dictionary<Symbol, TypeBuilder> types = new();
    private readonly Dictionary<Symbol, string> variables = new();
    private readonly StringBuilder globalsBuilder = new();
    private readonly Stack<StringBuilder> codeStack = new();
    private int tmpCount;
    private int labelCount;

    private StringBuilder CodeBuilder => this.codeStack.Peek();

    private void PushContext(StringBuilder sb) => this.codeStack.Push(sb);
    private void PopContext() => this.codeStack.Pop();

    private string TmpName() => $"_tmp_{this.tmpCount++}";
    private string LabelName() => $"_label_{this.labelCount++}";

    private string GetLocalName(Symbol symbol)
    {
        if (!this.variables.TryGetValue(symbol, out var name))
        {
            name = $"_local_{this.variables.Count}";
            this.variables.Add(symbol, name);
        }
        return name;
    }

    private TypeBuilder GetTypeBuilder(Symbol symbol)
    {
        var builderMap = symbol.Supertype is null
            ? this.types
            : this.GetTypeBuilder(symbol.Supertype).SubtypeBuilders;
        if (!builderMap.TryGetValue(symbol, out var builder))
        {
            builder = new()
            {
                Name = symbol.Name,
            };
            builderMap.Add(symbol, builder);
        }
        return builder;
    }

    private string GetTypeString(Expr expr) => expr switch
    {
        Expr.Name name => name.Symbol!.FullName,
        Expr.Index index => $"{this.GetTypeString(index.Indexed)}<{string.Join(", ", index.Indices.Select(GetTypeString))}>",
        Expr.MemberAccess maccess => $"{this.GetTypeString(maccess.Instance)}.{maccess.Member}",
        _ => throw new NotImplementedException(),
    };

    private static string DeVoid(string expr) => $"Prelude.DeVoid(() => {expr})";

    protected override string Visit(Decl.Record record)
    {
        var builder = this.GetTypeBuilder(record.Symbol!);

        // Properties
        foreach (var m in record.Members)
        {
            var ty = this.GetTypeString(m.Type);
            var getSet = m.Mutable ? "get; set;" : "get; init;";
            builder.CodeBuilder.AppendLine($"public {ty} {m.Name} {{ {getSet} }}");
        }

        // Ctor
        builder.CodeBuilder
            .Append($"public {record.Name}(")
            .AppendJoin(", ", record.Members.Select(m => $"{this.GetTypeString(m.Type)} {m.Name}"))
            .AppendLine(")")
            .AppendLine("{");
        foreach (var m in record.Members) builder.CodeBuilder.AppendLine($"this.{m.Name} = {m.Name};");
        builder.CodeBuilder.AppendLine("}");

        return this.Default;
    }

    protected override string Visit(Decl.Enum @enum)
    {
        var builder = this.GetTypeBuilder(@enum.Symbol!);
        builder.Abstract = true;

        // Subtypes
        foreach (var v in @enum.Variants) this.Visit(v);

        return this.Default;
    }

    protected override string Visit(Decl.EnumVariant enumVariant)
    {
        var baseBuilder = this.GetTypeBuilder(enumVariant.Symbol!.Supertype!);
        var builder = this.GetTypeBuilder(enumVariant.Symbol!);

        // Base type
        builder.Bases.Add(baseBuilder.Name);

        // Properties
        foreach (var m in enumVariant.Members)
        {
            var ty = this.GetTypeString(m.Type);
            var getSet = m.Mutable ? "get; set;" : "get; init;";
            builder.CodeBuilder.AppendLine($"public {ty} {m.Name} {{ {getSet} }}");
        }

        // Ctor
        builder.CodeBuilder
            .Append($"public {enumVariant.Name}(")
            .AppendJoin(", ", enumVariant.Members.Select(m => $"{this.GetTypeString(m.Type)} {m.Name}"))
            .AppendLine(")")
            .AppendLine("{");
        foreach (var m in enumVariant.Members) builder.CodeBuilder.AppendLine($"this.{m.Name} = {m.Name};");
        builder.CodeBuilder.AppendLine("}");

        return this.Default;
    }

    protected override string Visit(Decl.Impl impl)
    {
        var typeSymbol = impl.Target switch
        {
            Expr.Name n => n.Symbol!,
            _ => throw new NotImplementedException(),
        };

        var typeBuilder = this.GetTypeBuilder(typeSymbol);

        if (impl.Base is not null) typeBuilder.Bases.Add(this.GetTypeString(impl.Base));

        this.PushContext(typeBuilder.CodeBuilder);
        foreach (var decl in impl.Decls) this.Visit(decl);
        this.PopContext();

        return this.Default;
    }

    protected override string Visit(Decl.Func func)
    {
        var retType = func.Signature.Ret is null ? "void" : this.GetTypeString(func.Signature.Ret);
        var isInstance = func.Signature.Params.Count > 0 && func.Signature.Params[0].Name == "this";
        var isOverride = func.Attributes.Any(attr => attr.Name == "override");

        var relParams = isInstance ? func.Signature.Params.Skip(1) : func.Signature.Params;
        var stat = isInstance ? "" : "static ";
        var ov = isOverride ? "override " : "";
        this.CodeBuilder
            .Append($"public {ov}{stat}{retType} {func.Signature.Name}(")
            .AppendJoin(", ", relParams.Select(p => $"{this.GetTypeString(p.Type!)} {this.GetLocalName(p.Symbol!)}"))
            .AppendLine(")")
            .AppendLine("{");

        this.Visit(func.Body);

        this.CodeBuilder.AppendLine("}");

        return this.Default;
    }

    protected override string Visit(Decl.Var var)
    {
        if (var.Type is null && var.Value is null) throw new NotImplementedException();

        if (var.Scope!.IsGlobal)
        {
            if (var.Type is null) throw new NotImplementedException();
            // TODO
            throw new NotImplementedException();
        }

        var ty = var.Type is null ? "var" : this.GetTypeString(var.Type);
        var value = var.Value is null ? null : this.Visit(var.Value);
        var name = this.GetLocalName(var.Symbol!);

        if (value is null) this.CodeBuilder.AppendLine($"{ty} {name};");
        else this.CodeBuilder.AppendLine($"{ty} {name} = {value};");

        return this.Default;
    }

    protected override string Visit(Stmt.Return @return)
    {
        if (@return.Value is null)
        {
            this.CodeBuilder.AppendLine("return;");
        }
        else
        {
            var res = this.Visit(@return.Value);
            this.CodeBuilder.AppendLine($"return {res};");
        }
        return this.Default;
    }

    protected override string Visit(Expr.Block block)
    {
        foreach (var s in block.Stmts) this.Visit(s);
        return block.Value is null ? "default(Unit)" : this.Visit(block.Value);
    }

    protected override string Visit(Expr.If @if)
    {
        var res = this.TmpName();
        var cond = this.Visit(@if.Cond);
        var elseLabel = this.LabelName();
        var endLabel = this.LabelName();
        this.CodeBuilder.AppendLine($"if (!{cond}) goto {elseLabel};");
        var thenResult = this.Visit(@if.Then);
        this.CodeBuilder
            .AppendLine($"goto {endLabel};")
            .AppendLine($"{elseLabel}:;");
        var elseResult = @if.Else is null
            ? "default(Unit)"
            : this.Visit(@if.Else);
        this.CodeBuilder
            .AppendLine($"{endLabel}:;")
            .AppendLine($"var {res} = {cond} ? {DeVoid(thenResult)} : {DeVoid(elseResult)};");
        return res;
    }

    protected override string Visit(Expr.While @while)
    {
        var startLabel = this.LabelName();
        var endLabel = this.LabelName();
        this.CodeBuilder.AppendLine($"{startLabel}:;");
        var cond = this.Visit(@while.Cond);        
        this.CodeBuilder.AppendLine($"if (!{cond}) goto {endLabel};");
        this.Visit(@while.Body);
        this.CodeBuilder.AppendLine($"goto {startLabel};");
        this.CodeBuilder.AppendLine($"{endLabel}:;");
        return "default(Unit)";
    }

    protected override string Visit(Expr.Name name) => name.Symbol!.Kind == SymbolKind.Local
        ? this.GetLocalName(name.Symbol!)
        : name.Symbol!.FullName;

    protected override string Visit(Expr.Call call)
    {
        var isCtor = call.Called is Expr.Name name
            && name.Symbol!.Kind == SymbolKind.Type;

        var func = this.Visit(call.Called);
        var args = call.Args.Select(this.Visit).ToList();

        var res = this.TmpName();
        var callExpr = $"{func}({string.Join(", ", args)})";

        if (isCtor)
        {
            this.CodeBuilder.AppendLine($"var {res} = new {callExpr};");
        }
        else
        {
            this.CodeBuilder.AppendLine($"var {res} = {DeVoid(callExpr)};");
        }

        return res;
    }

    protected override string Visit(Expr.Bin bin)
    {
        var res = this.TmpName();
        var op = bin.Op switch
        {
            "and" => "&&",
            _ => bin.Op,
        };

        if (op == "&&")
        {
            this.CodeBuilder.AppendLine($"var {res} = false;");
            var left = this.Visit(bin.Left);
            this.CodeBuilder
                .AppendLine($"if ({left})")
                .AppendLine("{");
            var right = this.Visit(bin.Right);
            this.CodeBuilder
                .AppendLine($"if ({right})")
                .AppendLine("{");
            this.CodeBuilder.AppendLine($"{res} = true;");
            this.CodeBuilder
                .AppendLine("}")
                .AppendLine("}");
        }
        else if (op == "||")
        {
            this.CodeBuilder.AppendLine($"var {res} = false;");
            var left = this.Visit(bin.Left);
            this.CodeBuilder
                .AppendLine($"if ({left})")
                .AppendLine("{")
                .AppendLine($"{res} = true")
                .AppendLine("}")
                .AppendLine("else")
                .AppendLine("{");
            var right = this.Visit(bin.Right);
            this.CodeBuilder
                .AppendLine($"if ({right})")
                .AppendLine("{");
            this.CodeBuilder.AppendLine($"{res} = true;");
            this.CodeBuilder
                .AppendLine("}")
                .AppendLine("}");
        }
        else
        {
            var left = this.Visit(bin.Left);
            var right = this.Visit(bin.Right);

            this.CodeBuilder.AppendLine($"var {res} = {left} {op} {right};");
        }

        return res;
    }

    protected override string Visit(Expr.Rel rel)
    {
        var res = this.TmpName();
        this.CodeBuilder.AppendLine($"var {res} = false;");

        Debug.Assert(rel.Rights.Count > 0);
        var last = this.Visit(rel.Left);
        foreach (var (op, rightExp) in rel.Rights)
        {
            var right = this.Visit(rightExp);
            var tmp = this.TmpName();
            this.CodeBuilder
                .AppendLine($"var {tmp} = {last} {op} {right};")
                .AppendLine($"if ({tmp})")
                .AppendLine("{");
            last = right;
        }
        this.CodeBuilder.AppendLine($"{res} = true;");
        foreach (var _ in rel.Rights) this.CodeBuilder.AppendLine("}");

        return res;
    }

    protected override string Visit(Expr.Lit lit) => lit.Value;

    protected override string Visit(Expr.This @this) => "this";

    protected override string Visit(Expr.MemberAccess memberAccess) =>
        $"{this.Visit(memberAccess.Instance)}.{memberAccess.Member}";

    protected override string Visit(Expr.MemberCall memberCall)
    {
        var instance = this.Visit(memberCall.Instance);

        // Find out if this is really a ctor call
        var predictedTypeName = $"{instance}.{memberCall.Member}";
        // Either native type or a compound type name
        var isCtor = AppDomain.CurrentDomain.GetAssemblies().Any(a => a.GetType(predictedTypeName) is not null)
                  || memberCall.Scope!.ReferenceOpt(predictedTypeName) is not null;

        var args = memberCall.Args.Select(this.Visit).ToList();

        var res = this.TmpName();
        var callExpr = $"{instance}.{memberCall.Member}({string.Join(", ", args)})";

        if (isCtor)
        {
            this.CodeBuilder.AppendLine($"var {res} = new {callExpr};");
        }
        else
        {
            this.CodeBuilder.AppendLine($"var {res} = {DeVoid(callExpr)};");
        }

        return res;
    }
}
