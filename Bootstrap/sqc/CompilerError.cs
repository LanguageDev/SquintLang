// Copyright (c) 2022 SquintLang.
// Licensed under the Apache License, Version 2.0.
// Source repository: https://github.com/LanguageDev/SquintLang

using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Squint.Compiler;

public sealed class CompilerError : Exception
{
    [DoesNotReturn]
    public static void Report(string message) => throw new CompilerError(message);

    public CompilerError(string message)
        : base(message)
    {
    }
}
