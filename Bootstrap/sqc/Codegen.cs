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

    public string Code => throw new NotImplementedException();
}
