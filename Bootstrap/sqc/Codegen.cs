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
        public readonly record struct PropInfo(string Type, string Name, bool Mutable);

        public string Kind { get; set; } = "class";
        public bool Abstract { get; set; } = false;
        public bool Open { get; set; } = false;
        public string Name { get; set; } = "Unnamed";
        public HashSet<string> Bases { get; set; } = new();
        public StringBuilder CodeBuilder { get; set; } = new();
        public List<PropInfo> Properties { get; set; } = new();
        public Dictionary<Symbol, TypeBuilder> SubtypeBuilders { get; set; } = new();

        public string Code
        {
            get
            {
                var result = new StringBuilder();

                // Header
                result.Append("public ");
                if (this.Abstract) result.Append("abstract ");
                if (!this.Open && !this.Abstract) result.Append("sealed ");
                result.Append(this.Kind).Append(' ');
                result.Append(this.Name);
                if (this.Bases.Count > 0) result.Append(" : ").AppendJoin(", ", this.Bases);
                result.AppendLine();
                result.AppendLine("{");

                // Subtypes
                foreach (var sub in this.SubtypeBuilders.Values) result.AppendLine(sub.Code);

                // Properties
                foreach (var (ty, name, mut) in this.Properties)
                {
                    var getSet = mut ? "get; set;" : "get; init;";
                    result.AppendLine($"public {ty} {name} {{ {getSet} }}");
                }

                // Ctor
                result
                    .Append($"public {this.Name}(")
                    .AppendJoin(", ", this.Properties.Select(m => $"{m.Type} {m.Name}"))
                    .AppendLine(")")
                    .AppendLine("{");
                foreach (var m in this.Properties) result.AppendLine($"this.{m.Name} = {m.Name};");
                result.AppendLine("}");

                // Deconstruction
                // NOTE: For easier codegen destructure returns true
                result
                    .Append("public bool Deconstruct(")
                    .AppendJoin(", ", this.Properties.Select(m => $"out {m.Type} {m.Name}"))
                    .AppendLine(")")
                    .AppendLine("{");
                foreach (var m in this.Properties) result.AppendLine($"{m.Name} = this.{m.Name};");
                result
                    .AppendLine("return true;")
                    .AppendLine("}");

                // Custom code
                result.AppendLine(this.CodeBuilder.ToString().TrimEnd());

                // Close
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
    public static bool CopyOut<T>(T a, out T b)
    {{
        b = a;
        return true;
    }}
}}

public static class Program
{{
    public static void Main(string[] args)
    {{
        Globals.main();
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

    private static bool IsFunctionLocal(Scope scope) => scope.Kind switch
    {
        ScopeKind.Local => IsFunctionLocal(scope.Parent!),
        ScopeKind.Function => true,
        ScopeKind.Type => false,
        ScopeKind.Global => false,
        _ => throw new NotImplementedException(),
    };

    private static string EscapeKeyword(string str) => str switch
    {
        "new" => "@new",
        _ => str,
    };

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
        Expr.Index index => $"{this.GetTypeString(index.Indexed)}<{string.Join(", ", index.Indices.Select(this.GetTypeString))}>",
        Expr.MemberAccess maccess => $"{this.GetTypeString(maccess.Instance)}.{maccess.Member}",
        Expr.FuncType func => $"System.Func<{string.Join(", ", func.Params.Append(func.Return).Select(this.GetTypeString))}>",
        _ => throw new NotImplementedException(),
    };

    private bool IsType(Expr expr) => expr switch
    {
        Expr.Name name => name.Symbol!.Kind == SymbolKind.Type,
        Expr.Index i => this.IsType(i.Indexed) && i.Indices.All(this.IsType),
        _ => false,
    };

    private string TranslatePattern(Pattern p, string parent)
    {
        switch (p)
        {
        case Pattern.Destructure d:
        {
            var ty = d.NameSymbol!.FullName;
            var boundName = this.TmpName();
            var boundArgs = d.Args
                .Select(a => (Pattern: a, Name: this.TmpName()))
                .ToList();
            var outArgs = string.Join(", ", boundArgs.Select(b => $"out var {b.Name}"));
            var remPatterns = boundArgs
                .Select(a => this.TranslatePattern(a.Pattern, a.Name))
                .Where(a => !string.IsNullOrWhiteSpace(a))
                .ToList();
            var remPattern = string.Join("", remPatterns.Select(p => $" && {p}"));
            return $"({parent} is {ty} {boundName} && {boundName}.Deconstruct({outArgs}){remPattern})";
        }

        case Pattern.Name n:
        {
            var targetName = this.GetLocalName(n.Symbol!);
            return $"Prelude.CopyOut({parent}, out var {targetName})";
        }

        case Pattern.Discard:
            return "true";

        case Pattern.Literal lit:
            return $"{parent}.Equals({lit.Value})";

        default:
            throw new NotImplementedException();
        }
    }

    private static string DeVoid(string expr) => $"Prelude.DeVoid(() => {expr})";

    protected override string Visit(Decl.Record record)
    {
        var builder = this.GetTypeBuilder(record.Symbol!);

        // Properties
        foreach (var m in record.Members)
        {
            var ty = this.GetTypeString(m.Type);
            builder.Properties.Add(new(ty, m.Name, m.Mutable));
        }
        
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
            builder.Properties.Add(new(ty, m.Name, m.Mutable));
        }

        return this.Default;
    }

    protected override string Visit(Decl.Impl impl)
    {
        var typeSymbol = impl.Target switch
        {
            Expr.Name n => n.Symbol!,
            _ => impl.Scope!.ReferenceOpt(this.GetTypeString(impl.Target)) ?? throw new NotImplementedException(),
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
        var stat = isInstance ? "" : "static";
        var ov = isOverride ? "override" : "";
        var qualifiers = IsFunctionLocal(func.Scope!)
            ? ""
            : $"public {ov} {stat}";
        this.CodeBuilder
            .Append($"{qualifiers} {retType} {EscapeKeyword(func.Signature.Name)}(")
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

    protected override string Visit(Expr.For @for)
    {
        var enumerable = this.Visit(@for.Iterated);
        var enumerator = this.TmpName();
        this.CodeBuilder.AppendLine($"var {enumerator} = {enumerable}.GetEnumerator();");
        var startLabel = this.LabelName();
        var endLabel = this.LabelName();
        var itVar = this.GetLocalName(@for.IteratorSymbol!);
        this.CodeBuilder
            .AppendLine($"{startLabel}:;")
            .AppendLine($"if (!{enumerator}.MoveNext()) goto {endLabel};")
            .AppendLine($"var {itVar} = {enumerator}.Current;");
        this.Visit(@for.Body);
        this.CodeBuilder
            .AppendLine($"goto {startLabel};")
            .AppendLine($"{endLabel}:;");
        return "default(Unit)";
    }

    protected override string Visit(Expr.Match match)
    {
        var res = this.TmpName();
        var valueRes = this.Visit(match.Value);

        var choiceVar = this.TmpName();
        var armLabels = match.Arms
            .Select(_ => this.LabelName())
            .Append(this.LabelName())
            .ToList();
        var endLabel = this.LabelName();
        var armResults = new List<string>();
        this.CodeBuilder.AppendLine($"var {choiceVar} = -1;");

        for (var i = 0; i < match.Arms.Count; ++i)
        {
            if (i > 0) this.CodeBuilder.AppendLine($"{armLabels[i]}:;");
            var arm = match.Arms[i];
            var patternMatcher = this.TranslatePattern(arm.Pattern, valueRes);
            // If the pattern does not match, go to the next label
            this.CodeBuilder.AppendLine($"if (!{patternMatcher}) goto {armLabels[i + 1]};");
            // Otherwise do the thing in this arm
            // Also save that this was the choice that ran
            this.CodeBuilder.AppendLine($"{choiceVar} = {i};");
            armResults.Add(this.Visit(arm.Value));
            // Jump to the end
            this.CodeBuilder.AppendLine($"goto {endLabel};");
        }

        // Default label, we just throw
        this.CodeBuilder
            .AppendLine($"{armLabels[^1]}:")
            .AppendLine("throw new System.InvalidOperationException(\"Unhandled match case!\");");

        // End label
        this.CodeBuilder.AppendLine($"{endLabel}:");
        // We convince the compiler that everything is properly initialized
        foreach (var r in armResults) this.CodeBuilder.AppendLine($"System.Runtime.CompilerServices.Unsafe.SkipInit(out {r});");
        // Assign result value
        this.CodeBuilder
            .AppendLine($"var {res} = {choiceVar} switch")
            .AppendLine("{");
        for (var i = 0; i < match.Arms.Count; ++i) this.CodeBuilder.AppendLine($"{i} => {armResults[i]},");
        // Default and end
        this.CodeBuilder
            .AppendLine("_ => throw new System.InvalidOperationException(\"Unhandled match case!\"),")
            .AppendLine("};");

        return res;
    }

    protected override string Visit(Expr.Index index)
    {
        if (this.IsType(index.Indexed))
        {
            // Generic type
            return this.GetTypeString(index);
        }
        else
        {
            // Indexing
            var indexed = this.Visit(index.Indexed);
            var indices = index.Indices
                .Select(this.Visit)
                .ToList();
            return $"{indexed}[{string.Join(", ", indices)}]";
        }
    }

    protected override string Visit(Expr.Name name) => name.Symbol!.Kind == SymbolKind.Local
        ? this.GetLocalName(name.Symbol!)
        : name.Symbol!.FullName;

    protected override string Visit(Expr.Call call)
    {
        var isCtor = this.IsType(call.Called);

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

    protected override string Visit(Expr.StrLit strLit)
    {
        if (strLit.Pieces.Count == 1 && strLit.Pieces[0] is string)
        {
            // Optimize for a string literal
            return $"\"{strLit.Pieces[0]}\"";
        }
        else
        {
            // Build it up
            var builder = this.TmpName();
            var res = this.TmpName();

            this.CodeBuilder.AppendLine($"var {builder} = new System.Text.StringBuilder();");
            foreach (var p in strLit.Pieces)
            {
                if (p is Expr e)
                {
                    // Interpolated expression
                    var eRes = this.Visit(e);
                    this.CodeBuilder.AppendLine($"{builder}.Append({eRes}.ToString());");
                }
                else
                {
                    // Literal
                    this.CodeBuilder.AppendLine($"{builder}.Append(\"{p}\");");
                }
            }
            this.CodeBuilder.AppendLine($"var {res} = {builder}.ToString();");

            return res;
        }
    }

    protected override string Visit(Expr.This @this) => "this";

    protected override string Visit(Expr.MemberAccess memberAccess) =>
        $"{this.Visit(memberAccess.Instance)}.{EscapeKeyword(memberAccess.Member)}";

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
        var callExpr = $"{instance}.{EscapeKeyword(memberCall.Member)}({string.Join(", ", args)})";

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
