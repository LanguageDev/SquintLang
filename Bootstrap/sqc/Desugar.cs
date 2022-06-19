// Copyright (c) 2022 SquintLang.
// Licensed under the Apache License, Version 2.0.
// Source repository: https://github.com/LanguageDev/SquintLang

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Squint.Compiler;

public sealed class Desugar : AstTransformer
{
    private readonly record struct DeriveInfo(string TypeName, IEnumerable<string> MemberNames);

    public static Ast Trafo(Ast ast) => new Desugar().Transform(ast);

    private readonly Stack<Decl.Enum> enumStack = new();

    public override Decl Transform(Decl.Record record)
    {
        record = (Decl.Record)base.Transform(record);
        
        var derInfo = new DeriveInfo(record.Name, record.Members.Select(m => m.Name));
        return ApplyDerives(record, derInfo);
    }

    public override Decl Transform(Decl.Enum @enum)
    {
        this.enumStack.Push(@enum);
        // We attach each derive to the children
        var derives = GetDeriveArgs(@enum)
            .Select(a => new Decl.Attribute("derive", ImmutableList.Create(a)))
            .ToImmutableList();
        foreach (var v in @enum.Variants) v.Attributes = v.Attributes.Concat(derives).ToImmutableList();
        var result = base.Transform(@enum);
        this.enumStack.Pop();
        return result;
    }

    public override Decl Transform(Decl.EnumVariant enumVariant)
    {
        enumVariant = (Decl.EnumVariant)base.Transform(enumVariant);
        var parent = this.enumStack.Peek();

        // Go through each derive argument
        var fullName = $"{parent.Name}.{enumVariant.Name}";
        var derInfo = new DeriveInfo(fullName, enumVariant.Members.Select(m => m.Name));
        return ApplyDerives(enumVariant, derInfo);
    }

    private static Decl ApplyDerives(Decl decl, DeriveInfo deriveInfo)
    {
        // Go through each derive argument
        foreach (var derive in GetDeriveArgs(decl))
        {
            var impl = ImplementDerive(deriveInfo, derive);
            decl = new Decl.Seq(ImmutableList.Create(decl, impl));
        }
        return decl;
    }

    private static IEnumerable<Expr> GetDeriveArgs(Decl decl) =>
        decl.Attributes.Where(attr => attr.Name == "derive").SelectMany(d => d.Args);

    private static Decl ImplementDerive(DeriveInfo target, Expr der) => der switch
    {
        Expr.Name name when name.Value == "Equatable" => (Decl)Ast.Parse($@"
impl {target.TypeName} {{
    #[override]
    func Equals(this, other: object): bool = match other with
        | {target.TypeName} o -> this.Equals(o)
        | _ -> false
        ;

    #[override]
    func GetHashCode(this): int {{
        val h = System.HashCode();
        {string.Join("\n", target.MemberNames.Select(m => $"h.Add(this.{m});"))}
        return h.ToHashCode();
    }}
}}
impl System.IEquatable[{target.TypeName}] for {target.TypeName} {{
    func Equals(this, other: {target.TypeName}): bool =
        {(target.MemberNames.Any()
            ? string.Join(" and ", target.MemberNames.Select(m => $"this.{m}.Equals(other.{m})"))
            : "true")};
}}
"),

        Expr.Name name when name.Value == "ToString" => (Decl)Ast.Parse($@"
impl {target.TypeName} {{
    #[override]
    func ToString(this): string {{
        var sb = System.Text.StringBuilder();
        sb.Append(""{target.TypeName}("");
        {string.Join("sb.Append(\", \");", target.MemberNames.Select(m => $"sb.Append(this.{m}.ToString());"))}
        sb.Append("")"");
        return sb.ToString();
    }}
}}
"),
        _ => throw new NotImplementedException(),
    };
}
