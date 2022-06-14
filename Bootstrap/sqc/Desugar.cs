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
    public static Ast Trafo(Ast ast) => new Desugar().Transform(ast);

    public override Decl Transform(Decl.Record record)
    {
        record = (Decl.Record)base.Transform(record);
        var result = record as Decl;

        // Go through each derive argument
        foreach (var derive in record.Attributes.Where(attr => attr.Name == "derive").SelectMany(d => d.Args))
        {
            var impl = ImplementDerive(record, derive);
            result = new Decl.Seq(ImmutableList.Create(result, impl));
        }

        return result;
    }

    private static Decl ImplementDerive(Decl.ITypeDecl target, Expr der) => der switch
    {
        Expr.Name name when name.Value == "Equatable" => (Decl)Ast.Parse($@"
import System.IEquatable[T];

impl IEquatable[{target.Name}] for {target.Name} {{
    func Equals(this, other: {target.Name}): bool =
        {string.Join(" and ", target.Members.Select(m => $"this.{m.Name}.Equals(other.{m.Name})"))};
}}
"),
        Expr.Name name when name.Value == "ToString" => (Decl)Ast.Parse($@"
impl IEquatable[{target.Name}] for {target.Name} {{
    func ToString(this): string = ""TODO"";
}}
"),
        _ => throw new NotImplementedException(),
    };
}
