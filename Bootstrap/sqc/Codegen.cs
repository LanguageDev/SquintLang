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
        gen.Visit(ast);
        return gen.Code;
    }

    public sealed class TypeBuilder
    {
        public string Kind { get; set; } = "class";
        public bool Open { get; set; } = false;
        public string Name { get; set; } = "Unnamed";
        public HashSet<string> Bases { get; set; } = new();
        public StringBuilder Content { get; set; } = new();

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
                result.AppendLine(this.Content.ToString().TrimEnd());
                result.AppendLine("}");
                return result.ToString();
            }
        }
    }

    public string Code => string.Join("\n", this.types.Values.Select(b => b.Code));

    private readonly Dictionary<Symbol, TypeBuilder> types = new();

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
        Expr.Name name => name.Symbol!.Name,
        _ => throw new NotImplementedException(),
    };

    protected override string Visit(Decl.Record record)
    {
        var builder = this.GetTypeBuilder(record.Symbol!);

        foreach (var m in record.Members)
        {
            var ty = this.GetTypeString(m.Type);
            var getSet = m.Mutable ? "get; set;" : "get; init;";
            builder.Content.AppendLine($"    public {ty} {m.Name} {{ {getSet} }}");
        }

        return this.Default;
    }
}
