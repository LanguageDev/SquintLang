// Copyright (c) 2022 SquintLang.
// Licensed under the Apache License, Version 2.0.
// Source repository: https://github.com/LanguageDev/SquintLang

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Squint.Compiler;

public sealed class Codegen : AstVisitor<string>
{
    public static string Generate(Ast ast)
    {
        var gen = new Codegen();
        gen.PushContext(gen.globalsBuilder);
        gen.DumpPrelude();
        gen.Visit(ast);
        gen.PopContext();
        return gen.Code;
    }

    public sealed class TypeBuilder
    {
        public string Kind { get; set; } = "class";
        public bool Open { get; set; } = false;
        public string Name { get; set; } = "Unnamed";
        public HashSet<string> Bases { get; set; } = new();
        public StringBuilder CodeBuilder { get; set; } = new();

        public string Code
        {
            get
            {
                var result = new StringBuilder();
                result.Append("public ");
                if (!this.Open) result.Append("sealed ");
                result.Append(this.Kind).Append(' ');
                result.Append(this.Name);
                if (this.Bases.Count > 0) result.Append(" : ").AppendJoin(", ", this.Bases);
                result.AppendLine();
                result.AppendLine("{");
                result.AppendLine(this.CodeBuilder.ToString().TrimEnd());
                result.AppendLine("}");
                return result.ToString();
            }
        }
    }

    public string Code =>
        this.globalsBuilder.ToString() + "\n" +
        string.Join("\n", this.types.Values.Select(b => b.Code));

    private readonly Dictionary<Symbol, TypeBuilder> types = new();
    private readonly StringBuilder globalsBuilder = new();
    private readonly Stack<StringBuilder> codeStack = new();
    private int tmpCount;

    private StringBuilder CodeBuilder => this.codeStack.Peek();

    private void PushContext(StringBuilder sb) => this.codeStack.Push(sb);
    private void PopContext() => this.codeStack.Pop();

    private string TmpName() => $"_tmp_{this.tmpCount++}";

    private TypeBuilder GetTypeBuilder(Symbol symbol)
    {
        if (!this.types.TryGetValue(symbol, out var builder))
        {
            builder = new()
            {
                Name = symbol.Name,
            };
            this.types.Add(symbol, builder);
        }
        return builder;
    }

    private string GetTypeString(Expr expr) => expr switch
    {
        Expr.Name name => name.Symbol!.FullName,
        _ => throw new NotImplementedException(),
    };

    private string DeVoid(string expr) => $"Prelude.DeVoid(() => {expr})";

    private void DumpPrelude() => this.CodeBuilder
        .AppendLine(@"
public readonly struct Unit {}
public static class Prelude
{
    public static T DeVoid<T>(System.Func<T> f) => f();
    public static Unit DeVoid(System.Action f)
    {
        f();
        return default(Unit);
    }
}
".Trim());

    protected override string Visit(Decl.Record record)
    {
        var builder = this.GetTypeBuilder(record.Symbol!);

        foreach (var m in record.Members)
        {
            var ty = this.GetTypeString(m.Type);
            var getSet = m.Mutable ? "get; set;" : "get; init;";
            builder.CodeBuilder.AppendLine($"    public {ty} {m.Name} {{ {getSet} }}");
        }

        return this.Default;
    }

    protected override string Visit(Decl.Impl impl)
    {
        // TODO
        if (impl.Base is not null) throw new NotImplementedException();

        var typeSymbol = impl.Target switch
        {
            Expr.Name n => n.Symbol!,
            _ => throw new NotImplementedException(),
        };

        var typeBuilder = this.GetTypeBuilder(typeSymbol);
        this.PushContext(typeBuilder.CodeBuilder);
        foreach (var decl in impl.Decls) this.Visit(decl);
        this.PopContext();

        return this.Default;
    }

    protected override string Visit(Decl.Func func)
    {
        var retType = func.Signature.Ret is null ? "Unit" : this.GetTypeString(func.Signature.Ret);
        var isInstance = func.Signature.Params.Count > 0 && func.Signature.Params[0].Name == "this";

        var relParams = isInstance ? func.Signature.Params.Skip(1) : func.Signature.Params;
        var stat = isInstance ? "" : "static ";
        this.CodeBuilder
            .Append($"public {stat}{retType} {func.Signature.Name}(")
            .AppendJoin(", ", relParams.Select(p => $"{this.GetTypeString(p.Type!)} {p.Name}"))
            .AppendLine(")")
            .AppendLine("{");

        this.Visit(func.Body);

        if (retType == "Unit") this.CodeBuilder.AppendLine("return default(Unit);");

        this.CodeBuilder.AppendLine("}");

        return this.Default;
    }

    protected override string Visit(Expr.Name name) => name.Symbol!.FullName;

    protected override string Visit(Expr.Call call)
    {
        var func = this.Visit(call.Called);
        var args = call.Args.Select(this.Visit).ToList();

        var res = this.TmpName();
        var callExpr = $"{func}({string.Join(", ", args)})";
        this.CodeBuilder.AppendLine($"var {res} = {DeVoid(callExpr)};");

        return res;
    }

    protected override string Visit(Expr.Bin bin)
    {
        var left = this.Visit(bin.Left);
        var right = this.Visit(bin.Right);

        var res = this.TmpName();
        this.CodeBuilder.AppendLine($"var {res} = {left} {bin.Op} {right};");

        return res;
    }

    protected override string Visit(Expr.Lit lit) => lit.Value;

    protected override string Visit(Expr.This @this) => "this";

    protected override string Visit(Expr.MemberAccess memberAccess) =>
        $"{this.Visit(memberAccess.Instance)}.{memberAccess.Member}";
}
