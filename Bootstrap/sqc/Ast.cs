// Copyright (c) 2022 SquintLang.
// Licensed under the Apache License, Version 2.0.
// Source repository: https://github.com/LanguageDev/SquintLang

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Compiler.Syntax;

namespace Squint.Compiler;

public abstract record class Ast
{
    public Scope? Scope { get; set; }
    public ImmutableList<Decl.Attribute> Attributes { get; set; } = ImmutableList<Decl.Attribute>.Empty;

    private static IParseTree ParseParseTree(string text)
    {
        var inputStream = new AntlrInputStream(text);
        var lexer = new SquintLexer(inputStream);
        var commonTokenStream = new CommonTokenStream(lexer);
        var parser = new SquintParser(commonTokenStream);
        return parser.file();
    }

    public static Ast Parse(string text)
    {
        var parseTree = ParseParseTree(text);
        var ast = AstConverter.ToAst(parseTree);
        ast = Desugar.Trafo(ast);
        return ast;
    }
}

public abstract record class Stmt : Ast
{
    public sealed record class Return(Expr? Value) : Stmt;
    public sealed record class Exp(Expr Expr) : Stmt;
}

public abstract record class Decl : Stmt
{
    public sealed record class Module(ImmutableList<Decl> Decls) : Decl;
    public sealed record class Seq(ImmutableList<Decl> Decls) : Decl;
    public sealed record class Attribute(string Name, ImmutableList<Expr> Args) : Decl;
    public sealed record class Import(
        ImmutableList<string> Parts,
        ImmutableList<GenericParam> Generics) : Decl;
    public sealed record class FuncSignature(
        string Name,
        ImmutableList<GenericParam> Generics,
        ImmutableList<FuncParam> Params,
        Expr? Ret) : Decl
    {
        public Symbol? Symbol { get; set; }
    }
    public sealed record class Func(FuncSignature Signature, Stmt Body) : Decl;
    public sealed record class GenericParam(string Name) : Decl;
    public sealed record class FuncParam(string Name, Expr? Type) : Decl
    {
        public Symbol? Symbol { get; set; }
    }
    public sealed record class TypeMember(bool Mutable, string Name, Expr Type) : Decl;
    public sealed record class Record(
        string Name,
        ImmutableList<GenericParam> Generics,
        ImmutableList<TypeMember> Members) : Decl
    {
        public Symbol? Symbol { get; set; }
    }
    public sealed record class Enum(
        string Name,
        ImmutableList<GenericParam> Generics,
        ImmutableList<EnumVariant> Variants) : Decl
    {
        public Symbol? Symbol { get; set; }
    }
    public sealed record class EnumVariant(
        string Name,
        ImmutableList<TypeMember> Members) : Decl
    {
        public Enum? Parent { get; set; }
        public Symbol? Symbol { get; set; }
    }
    public sealed record class Impl(
        Expr Target,
        Expr? Base,
        ImmutableList<Decl> Decls) : Decl;
    public sealed record class Var(bool Mutable, string Name, Expr? Type, Expr? Value) : Decl
    {
        public Symbol? Symbol { get; set; }
    }
}

public abstract record class Expr : Ast
{
    public sealed record class This : Expr;
    public sealed record class Name(string Value) : Expr
    {
        public Symbol? Symbol { get; set; }
    }
    public sealed record class Block(ImmutableList<Stmt> Stmts, Expr? Value) : Expr;
    public sealed record class If(Expr Cond, Expr Then, Expr? Else) : Expr;
    public sealed record class While(Expr Cond, Expr Body) : Expr;
    public sealed record class Call(Expr Called, ImmutableList<Expr> Args) : Expr;
    public sealed record class Ury(string Op, Expr Subexpr) : Expr;
    public sealed record class Bin(string Op, Expr Left, Expr Right) : Expr;
    public sealed record class Rel(Expr Left, ImmutableList<(string Op, Expr Right)> Rights) : Expr;
    public sealed record class Lit(string Value) : Expr;
    public sealed record class MemberAccess(Expr Instance, string Member) : Expr;
    public sealed record class MemberCall(Expr Instance, string Member, ImmutableList<Expr> Args) : Expr;
    public sealed record class Index(Expr Indexed, ImmutableList<Expr> Indices) : Expr;
    public sealed record class Match(Expr Value, ImmutableList<MatchArm> Arms) : Expr;
    public sealed record class MatchArm(Pattern Pattern, Expr Value) : Expr;
}

public abstract record class Pattern : Ast
{
    public sealed record class Name(string Value) : Pattern;
    public sealed record class Destructure(string Name_, ImmutableList<Pattern> Args) : Pattern;
}

public static class AstConverter
{
    public static Ast ToAst(IParseTree tree) => tree switch
    {
        SquintParser.FileContext file => ToDecl(file),
        _ => throw new NotImplementedException(),
    };

    private static Decl ToDecl(IParseTree tree) => tree switch
    {
        SquintParser.DeclarationContext decl => ToDecl(decl.GetChild(0)),
        SquintParser.Type_declarationContext decl => ToDecl(decl.GetChild(0)),
        SquintParser.Impl_member_declarationContext mem => ToDecl(mem.GetChild(0)),
        SquintParser.FileContext file => new Decl.Module(file.declaration().Select(ToDecl).ToImmutableList()),
        SquintParser.Import_declarationContext import => new Decl.Import(
            import.name().Select(n => n.GetText()).ToImmutableList(),
            ToGenerics(import.generic_param_list())),
        SquintParser.Record_type_declarationContext rec => new Decl.Record(
            rec.name().GetText(),
            ToGenerics(rec.generic_param_list()),
            rec.type_declaration_member_list().type_declaration_member().Select(ToDecl).Cast<Decl.TypeMember>().ToImmutableList())
            {
                Attributes = ToAttributeList(rec.attribute_list()),
            },
        SquintParser.Du_type_declarationContext du => ConnectVariants(new Decl.Enum(
            du.name().GetText(),
            ToGenerics(du.generic_param_list()),
            du.du_type_ctor().Select(ToDecl).Cast<Decl.EnumVariant>().ToImmutableList())
            {
                Attributes = ToAttributeList(du.attribute_list()),
            }),
        SquintParser.Du_type_ctorContext ctor => new Decl.EnumVariant(
            ctor.name().GetText(),
            ctor.type_declaration_member_list().type_declaration_member().Select(ToDecl).Cast<Decl.TypeMember>().ToImmutableList())
            {
                Attributes = ToAttributeList(ctor.attribute_list()),
            },
        SquintParser.Type_declaration_memberContext mem => new Decl.TypeMember(
            mem.GetChild(0).GetText() == "var",
            mem.name().GetText(),
            ToType(mem.type())),
        SquintParser.Impl_declarationContext impl => impl.type().Length == 2
            ? new Decl.Impl(ToType(impl.type(1)), ToType(impl.type(0)), impl.impl_member_declaration().Select(ToDecl).ToImmutableList())
            : new Decl.Impl(ToType(impl.type(0)), null, impl.impl_member_declaration().Select(ToDecl).ToImmutableList()),
        SquintParser.Function_signatureContext s => new Decl.FuncSignature(
            s.name().GetText(),
            ToGenerics(s.generic_param_list()),
            s.parameter_list().parameter().Select(ToDecl).Cast<Decl.FuncParam>().ToImmutableList(),
            s.return_type is null ? null : ToType(s.return_type)),
        SquintParser.Function_declarationContext f => f.expression() is null
            ? new Decl.Func(
                (Decl.FuncSignature)ToDecl(f.function_signature()),
                ToStmt(f.block_statement()))
                {
                    Attributes = ToAttributeList(f.attribute_list()),
                }
            : new Decl.Func(
                (Decl.FuncSignature)ToDecl(f.function_signature()),
                new Stmt.Exp(new Expr.Block(
                    ImmutableList.Create<Stmt>(new Stmt.Return(ToExpr(f.expression()))),
                    null)))
                    {
                        Attributes = ToAttributeList(f.attribute_list()),
                    },
        SquintParser.This_parameterContext => new Decl.FuncParam("this", null),
        SquintParser.Typed_parameterContext t => new Decl.FuncParam(
            t.name().GetText(),
            ToType(t.type())),
        SquintParser.Variable_declarationContext v => new Decl.Var(
            v.GetChild(0).GetText() == "var",
            v.name().GetText(),
            v.type() is null ? null : ToType(v.type()),
            v.expression() is null ? null : ToExpr(v.expression())),
        _ => throw new NotImplementedException(),
    };

    private static Stmt ToStmt(IParseTree tree) => tree switch
    {
        SquintParser.Declaration_statementContext d => ToDecl(d.GetChild(0)),
        SquintParser.Keep_statementContext d => ToStmt(d.GetChild(0)),
        SquintParser.Expression_statementContext e => new Stmt.Exp(ToExpr(e.expression())),
        SquintParser.Block_statementContext b => new Stmt.Exp(new Expr.Block(
            b.statement().Select(ToStmt).ToImmutableList(),
            null)),
        SquintParser.Return_statementContext r => new Stmt.Return(
            r.expression() is null ? null : ToExpr(r.expression())),
        SquintParser.If_statementContext i => new Stmt.Exp(new Expr.If(
            ToExpr(i.condition),
            StmtToExpr(i.then),
            StmtToExpr(i.els))),
        SquintParser.While_statementContext w => new Stmt.Exp(new Expr.While(
            ToExpr(w.condition),
            StmtToExpr(w.body))),
        SquintParser.Match_statementContext m => new Stmt.Exp(new Expr.Match(
            ToExpr(m.expression()),
            m.match_statement_arm().Select(ToExpr).Cast<Expr.MatchArm>().ToImmutableList())),
        _ => throw new NotImplementedException(),
    };

    private static Expr ToExpr(IParseTree tree) => tree switch
    {
        SquintParser.Grouping_expressionContext grouping => ToExpr(grouping.expression()),
        SquintParser.Wrapped_expressionContext wrapped => ToExpr(wrapped.GetChild(0)),
        SquintParser.Atom_expressionContext atom => ToExpr(atom.GetChild(0)),
        SquintParser.NameContext name => new Expr.Name(name.GetText()),
        SquintParser.Int_literalContext intLit => new Expr.Lit(intLit.GetText()),
        SquintParser.Bool_literalContext boolLit => new Expr.Lit(boolLit.GetText()),
        SquintParser.Str_literalContext strLit => new Expr.Lit(strLit.GetText()),
        SquintParser.Call_expressionContext call => new Expr.Call(
            ToExpr(call.func),
            call.args.expression().Select(ToExpr).ToImmutableList()),
        SquintParser.Ury_expressionContext ury => new Expr.Ury(
            ury.op.Text,
            ToExpr(ury.subexpr)),
        SquintParser.Bin_expressionContext bin => new Expr.Bin(
            bin.op.Text,
            ToExpr(bin.left),
            ToExpr(bin.right)),
        SquintParser.Member_access_expressionContext mAccess => new Expr.MemberAccess(
            ToExpr(mAccess.obj),
            mAccess.member.GetText()),
        SquintParser.Member_call_expressionContext mCall => new Expr.MemberCall(
            ToExpr(mCall.obj),
            mCall.member.GetText(),
            mCall.args.expression().Select(ToExpr).ToImmutableList()),
        ITerminalNode term when term.GetText() == "this" => new Expr.This(),
        SquintParser.Relational_expressionContext rel => ToRelExpr(rel),
        SquintParser.Block_expressionContext block => new Expr.Block(
            block.statement().Select(ToStmt).ToImmutableList(),
            block.expression() is null ? null : ToExpr(block.expression())),
        SquintParser.If_expressionContext i => new Expr.If(
            ToExpr(i.condition),
            ToExpr(i.then),
            i.els is null ? null : ToExpr(i.els)),
        SquintParser.While_expressionContext w => new Expr.While(
            ToExpr(w.condition),
            ToExpr(w.body)),
        SquintParser.Assign_expressionContext asgn => new Expr.Bin(
            asgn.op.GetText(),
            ToExpr(asgn.left),
            ToExpr(asgn.right)),
        SquintParser.Match_expressionContext m => new Expr.Match(
            ToExpr(m.expression()),
            m.match_expression_arm().Select(ToExpr).Cast<Expr.MatchArm>().ToImmutableList()),
        SquintParser.Match_expression_armContext a => new Expr.MatchArm(
            ToPattern(a.pattern()),
            ToExpr(a.expression())),
        SquintParser.Match_statement_armContext a => new Expr.MatchArm(
            ToPattern(a.pattern()),
            new Expr.Block(ImmutableList.Create(ToStmt(a.statement())), null)),
        _ => throw new NotImplementedException(),
    };

    private static Expr ToType(IParseTree tree) => tree switch
    {
        SquintParser.Name_typeContext n => new Expr.Name(n.GetText()),
        SquintParser.Generic_typeContext genType => new Expr.Index(
            ToType(genType.type()),
            genType.generic_arg_list().type().Select(ToType).ToImmutableList()),
        SquintParser.Nested_typeContext n => new Expr.MemberAccess(
            ToType(n.type()),
            n.name().GetText()),
        _ => throw new NotImplementedException(),
    };

    private static Pattern ToPattern(IParseTree tree) => tree switch
    {
        SquintParser.Name_patternContext n => new Pattern.Name(n.name().GetText()),
        SquintParser.Destructure_patternContext d => new Pattern.Destructure(
            string.Join('.', d.name().Select(n => n.GetText())),
            d.pattern_list().pattern().Select(ToPattern).ToImmutableList()),
        _ => throw new NotImplementedException(),
    };

    private static ImmutableList<Decl.GenericParam> ToGenerics(SquintParser.Generic_param_listContext? l) => l is null
        ? ImmutableList<Decl.GenericParam>.Empty
        : l.name().Select(n => new Decl.GenericParam(n.GetText())).ToImmutableList();

    private static Expr ToRelExpr(SquintParser.Relational_expressionContext tree)
    {
        var exprs = new List<Expr>();
        var ops = new List<string>();

        void Traverse(IParseTree tree)
        {
            if (tree is SquintParser.Relational_expressionContext rel)
            {
                Traverse(rel.left);
                ops!.Add((rel.op.GetText()));
                Traverse(rel.right);
            }
            else
            {
                exprs!.Add(ToExpr(tree));
            }
        }

        Traverse(tree);

        Debug.Assert(exprs.Count >= 2);
        Debug.Assert(ops.Count >= 1);
        Debug.Assert(exprs.Count - 1 == ops.Count);

        var left = exprs[0];
        var rights = ImmutableList.CreateBuilder<(string, Expr)>();
        for (var i = 0; i < ops.Count; ++i) rights.Add((ops[i], exprs[i + 1]));

        return new Expr.Rel(left, rights.ToImmutable());
    }

    private static ImmutableList<Decl.Attribute> ToAttributeList(SquintParser.Attribute_listContext attrs) => attrs is null
        ? ImmutableList<Decl.Attribute>.Empty
        : attrs.attribute_sequence().SelectMany(s => s.attribute()).Select(ToAttribute).ToImmutableList();

    private static Decl.Attribute ToAttribute(SquintParser.AttributeContext attr)
    {
        var args = ImmutableList<Expr>.Empty;
        if (attr.expression_list() is not null) args = attr.expression_list().expression().Select(ToExpr).ToImmutableList();
        if (attr.type_list() is not null) args = attr.type_list().type().Select(ToType).ToImmutableList();
        return new(attr.name().GetText(), args);
    }

    private static Expr StmtToExpr(IParseTree? tree)
    {
        if (tree is null) return new Expr.Block(ImmutableList<Stmt>.Empty, null);

        var stmt = ToStmt(tree);
        return new Expr.Block(ImmutableList.Create(stmt), null);
    }

    private static Decl ConnectVariants(Decl.Enum @enum)
    {
        foreach (var v in @enum.Variants) v.Parent = @enum;
        return @enum;
    }
}

public abstract class AstVisitor<TResult>
{
    public virtual TResult Default => default!;

    protected virtual TResult Visit(Ast ast) => ast switch
    {
        Decl v => this.Visit(v),
        Stmt v => this.Visit(v),
        Expr v => this.Visit(v),
        Pattern v => this.Visit(v),
        _ => throw new NotImplementedException(),
    };

    protected virtual TResult Visit(Stmt stmt) => stmt switch
    {
        Decl v => this.Visit(v),
        Stmt.Exp v => this.Visit(v),
        Stmt.Return v => this.Visit(v),
        _ => throw new NotImplementedException(),
    };

    protected virtual TResult Visit(Decl decl) => decl switch
    {
        Decl.Module v => this.Visit(v),
        Decl.Seq v => this.Visit(v),
        Decl.Import v => this.Visit(v),
        Decl.Record v => this.Visit(v),
        Decl.Enum v => this.Visit(v),
        Decl.EnumVariant v => this.Visit(v),
        Decl.TypeMember v => this.Visit(v),
        Decl.Impl v => this.Visit(v),
        Decl.Func v => this.Visit(v),
        Decl.FuncSignature v => this.Visit(v),
        Decl.FuncParam v => this.Visit(v),
        Decl.Var v => this.Visit(v),
        _ => throw new NotImplementedException(),
    };

    protected virtual TResult Visit(Expr expr) => expr switch
    {
        Expr.Name v => this.Visit(v),
        Expr.Block v => this.Visit(v),
        Expr.Call v => this.Visit(v),
        Expr.Bin v => this.Visit(v),
        Expr.Rel v => this.Visit(v),
        Expr.Lit v => this.Visit(v),
        Expr.MemberAccess v => this.Visit(v),
        Expr.MemberCall v => this.Visit(v),
        Expr.This v => this.Visit(v),
        Expr.Index v => this.Visit(v),
        Expr.If v => this.Visit(v),
        Expr.While v => this.Visit(v),
        Expr.Match v => this.Visit(v),
        Expr.MatchArm v => this.Visit(v),
        _ => throw new NotImplementedException(),
    };

    protected virtual TResult Visit(Pattern pattern) => pattern switch
    {
        Pattern.Name v => this.Visit(v),
        Pattern.Destructure v => this.Visit(v),
        _ => throw new NotImplementedException(),
    };

    protected virtual TResult Visit(Decl.Module module)
    {
        this.VisitAll(module.Decls);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.Seq seq)
    {
        this.VisitAll(seq.Decls);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.Import import) => this.Default;

    protected virtual TResult Visit(Decl.Record record)
    {
        this.VisitAll(record.Generics);
        this.VisitAll(record.Members);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.Enum @enum)
    {
        this.VisitAll(@enum.Generics);
        this.VisitAll(@enum.Variants);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.EnumVariant enumVariant)
    {
        this.VisitAll(enumVariant.Members);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.TypeMember typeMember)
    {
        this.Visit(typeMember.Type);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.Impl impl)
    {
        this.Visit(impl.Target);
        this.VisitOpt(impl.Base);
        this.VisitAll(impl.Decls);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.Func func)
    {
        this.Visit(func.Signature as Decl);
        this.Visit(func.Body);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.FuncSignature funcSignature)
    {
        this.VisitAll(funcSignature.Generics);
        this.VisitAll(funcSignature.Params);
        this.VisitOpt(funcSignature.Ret);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.FuncParam funcParam)
    {
        this.VisitOpt(funcParam.Type);
        return this.Default;
    }

    protected virtual TResult Visit(Decl.Var var)
    {
        this.VisitOpt(var.Type);
        this.VisitOpt(var.Value);
        return this.Default;
    }

    protected virtual TResult Visit(Stmt.Return @return)
    {
        this.VisitOpt(@return.Value);
        return this.Default;
    }

    protected virtual TResult Visit(Stmt.Exp exp)
    {
        this.Visit(exp.Expr);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.Name name) => this.Default;
    protected virtual TResult Visit(Expr.Lit lit) => this.Default;
    protected virtual TResult Visit(Expr.This @this) => this.Default;

    protected virtual TResult Visit(Expr.Block block)
    {
        this.VisitAll(block.Stmts);
        this.VisitOpt(block.Value);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.Call call)
    {
        this.Visit(call.Called);
        this.VisitAll(call.Args);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.Bin bin)
    {
        this.Visit(bin.Left);
        this.Visit(bin.Right);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.Rel rel)
    {
        this.Visit(rel.Left);
        this.VisitAll(rel.Rights.Select(r => r.Right));
        return this.Default;
    }

    protected virtual TResult Visit(Expr.MemberAccess memberAccess)
    {
        this.Visit(memberAccess.Instance);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.MemberCall memberCall)
    {
        this.Visit(memberCall.Instance);
        this.VisitAll(memberCall.Args);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.Index index)
    {
        this.Visit(index.Indexed);
        this.VisitAll(index.Indices);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.If @if)
    {
        this.Visit(@if.Cond);
        this.Visit(@if.Then);
        this.VisitOpt(@if.Else);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.While @while)
    {
        this.Visit(@while.Cond);
        this.Visit(@while.Body);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.Match match)
    {
        this.Visit(match.Value);
        this.VisitAll(match.Arms);
        return this.Default;
    }

    protected virtual TResult Visit(Expr.MatchArm matchArm)
    {
        this.Visit(matchArm.Pattern);
        this.Visit(matchArm.Value);
        return this.Default;
    }

    protected virtual TResult Visit(Pattern.Name name) => this.Default;

    protected virtual TResult Visit(Pattern.Destructure destructure)
    {
        this.VisitAll(destructure.Args);
        return this.Default;
    }

    private void VisitAll(IEnumerable<Stmt> stmts)
    {
        foreach (var d in stmts) this.Visit(d);
    }

    private void VisitAll(IEnumerable<Decl> decls)
    {
        foreach (var d in decls) this.Visit(d);
    }

    private void VisitAll(IEnumerable<Expr> exprs)
    {
        foreach (var d in exprs) this.Visit(d);
    }

    private void VisitAll(IEnumerable<Pattern> pats)
    {
        foreach (var d in pats) this.Visit(d);
    }

    private void VisitOpt(Expr? expr)
    {
        if (expr is not null) this.Visit(expr);
    }
}

public abstract class AstTransformer
{
    public virtual Ast Transform(Ast ast) => ast switch
    {
        Decl v => this.Transform(v),
        Stmt v => this.Transform(v),
        Expr v => this.Transform(v),
        Pattern v => this.Transform(v),
        _ => throw new NotImplementedException(),
    };

    public virtual Stmt Transform(Stmt stmt) => stmt switch
    {
        Decl v => this.Transform(v),
        Stmt.Exp v => this.Transform(v),
        Stmt.Return v => this.Transform(v),
        _ => throw new NotImplementedException(),
    };

    public virtual Decl Transform(Decl decl) => decl switch
    {
        Decl.Module v => this.Transform(v),
        Decl.Import v => this.Transform(v),
        Decl.Record v => this.Transform(v),
        Decl.Enum v => this.Transform(v),
        Decl.EnumVariant v => this.Transform(v),
        Decl.TypeMember v => this.Transform(v),
        Decl.Impl v => this.Transform(v),
        Decl.Func v => this.Transform(v),
        Decl.FuncSignature v => this.Transform(v),
        Decl.FuncParam v => this.Transform(v),
        Decl.Var v => this.Transform(v),
        _ => throw new NotImplementedException(),
    };

    public virtual Expr Transform(Expr expr) => expr switch
    {
        Expr.Name v => this.Transform(v),
        Expr.Block v => this.Transform(v),
        Expr.Call v => this.Transform(v),
        Expr.Bin v => this.Transform(v),
        Expr.Rel v => this.Transform(v),
        Expr.Lit v => this.Transform(v),
        Expr.MemberAccess v => this.Transform(v),
        Expr.MemberCall v => this.Transform(v),
        Expr.This v => this.Transform(v),
        Expr.Index v => this.Transform(v),
        Expr.If v => this.Transform(v),
        Expr.While v => this.Transform(v),
        Expr.Match v => this.Transform(v),
        Expr.MatchArm v => this.Transform(v),
        _ => throw new NotImplementedException(),
    };

    public virtual Pattern Transform(Pattern pattern) => pattern switch
    {
        Pattern.Name v => this.Transform(v),
        Pattern.Destructure v => this.Transform(v),
        _ => throw new NotImplementedException(),
    };

    public virtual Decl Transform(Decl.Module module) =>
        new Decl.Module(this.TransformAll(module.Decls));

    public virtual Decl Transform(Decl.Import import) => import;

    public virtual Decl Transform(Decl.Record record) => KeepAttr(record, new Decl.Record(
        record.Name,
        this.TransformAll(record.Generics).Cast<Decl.GenericParam>().ToImmutableList(),
        this.TransformAll(record.Members).Cast<Decl.TypeMember>().ToImmutableList()));

    public virtual Decl Transform(Decl.Enum @enum)
    {
        // The variants need to propagate decl sequences
        var (variants, others) = FilterSeq<Decl.EnumVariant>(this.TransformAll(@enum.Variants));
        var newSeq = KeepAttr(@enum, new Decl.Enum(
            @enum.Name,
            this.TransformAll(@enum.Generics).Cast<Decl.GenericParam>().ToImmutableList(),
            variants));
        foreach (var v in variants) v.Parent = newSeq;
        return new Decl.Seq(others.Append(newSeq).ToImmutableList());
    }

    public virtual Decl Transform(Decl.EnumVariant enumVariant) => KeepAttr(enumVariant, new Decl.EnumVariant(
        enumVariant.Name,
        this.TransformAll(enumVariant.Members).Cast<Decl.TypeMember>().ToImmutableList()));

    public virtual Decl Transform(Decl.TypeMember typeMember) =>
        new Decl.TypeMember(typeMember.Mutable, typeMember.Name, this.Transform(typeMember.Type));

    public virtual Decl Transform(Decl.Impl impl) => new Decl.Impl(
        this.Transform(impl.Target),
        this.TransformOpt(impl.Base),
        this.TransformAll(impl.Decls));

    public virtual Decl Transform(Decl.Func func) => KeepAttr(func, new Decl.Func(
        (Decl.FuncSignature)this.Transform(func.Signature),
        this.Transform(func.Body)));

    public virtual Decl Transform(Decl.FuncSignature funcSignature) => new Decl.FuncSignature(
        funcSignature.Name,
        this.TransformAll(funcSignature.Generics).Cast<Decl.GenericParam>().ToImmutableList(),
        this.TransformAll(funcSignature.Params).Cast<Decl.FuncParam>().ToImmutableList(),
        this.TransformOpt(funcSignature.Ret));
    
    public virtual Decl Transform(Decl.FuncParam funcParam) =>
        new Decl.FuncParam(funcParam.Name, this.TransformOpt(funcParam.Type));

    public virtual Decl Transform(Decl.Var var) =>
        new Decl.Var(var.Mutable, var.Name, this.TransformOpt(var.Type), this.TransformOpt(var.Value));

    public virtual Stmt Transform(Stmt.Return @return) => new Stmt.Return(this.TransformOpt(@return.Value));

    public virtual Stmt Transform(Stmt.Exp exp) => new Stmt.Exp(this.Transform(exp.Expr));

    public virtual Expr Transform(Expr.Name name) => name;
    public virtual Expr Transform(Expr.Lit lit) => lit;
    public virtual Expr Transform(Expr.This @this) => @this;

    public virtual Expr Transform(Expr.Block block) => new Expr.Block(
        this.TransformAll(block.Stmts),
        this.TransformOpt(block.Value));

    public virtual Expr Transform(Expr.Call call) => new Expr.Call(
        this.Transform(call.Called),
        this.TransformAll(call.Args));
    
    public virtual Expr Transform(Expr.Bin bin) => new Expr.Bin(
        bin.Op,
        this.Transform(bin.Left),
        this.Transform(bin.Right));

    public virtual Expr Transform(Expr.Rel rel) => new Expr.Rel(
        this.Transform(rel.Left),
        rel.Rights.Select(r => (r.Op, this.Transform(r.Right))).ToImmutableList());

    public virtual Expr Transform(Expr.MemberAccess memberAccess) => new Expr.MemberAccess(
        this.Transform(memberAccess.Instance),
        memberAccess.Member);

    public virtual Expr Transform(Expr.MemberCall memberCall) => new Expr.MemberCall(
        this.Transform(memberCall.Instance),
        memberCall.Member,
        this.TransformAll(memberCall.Args));

    public virtual Expr Transform(Expr.Index index) => new Expr.Index(
        this.Transform(index.Indexed),
        this.TransformAll(index.Indices));

    public virtual Expr Transform(Expr.If @if) => new Expr.If(
        this.Transform(@if.Cond),
        this.Transform(@if.Then),
        this.TransformOpt(@if.Else));

    public virtual Expr Transform(Expr.While @while) => new Expr.While(
        this.Transform(@while.Cond),
        this.Transform(@while.Body));

    public virtual Expr Transform(Expr.Match match) => new Expr.Match(
        this.Transform(match.Value),
        this.TransformAll(match.Arms).Cast<Expr.MatchArm>().ToImmutableList());

    public virtual Expr Transform(Expr.MatchArm matchArm) => new Expr.MatchArm(
        this.Transform(matchArm.Pattern),
        this.Transform(matchArm.Value));

    public virtual Pattern Transform(Pattern.Name name) => name;

    public virtual Pattern Transform(Pattern.Destructure destructure) => new Pattern.Destructure(
        destructure.Name_,
        this.TransformAll(destructure.Args));

    private ImmutableList<Stmt> TransformAll(IEnumerable<Stmt> stmts) =>
        stmts.Select(this.Transform).ToImmutableList();

    private ImmutableList<Decl> TransformAll(IEnumerable<Decl> decls) =>
        decls.Select(this.Transform).ToImmutableList();

    private ImmutableList<Expr> TransformAll(IEnumerable<Expr> exprs) =>
        exprs.Select(this.Transform).ToImmutableList();

    private ImmutableList<Pattern> TransformAll(IEnumerable<Pattern> pats) =>
        pats.Select(this.Transform).ToImmutableList();

    private Expr? TransformOpt(Expr? expr) => expr is null
        ? null
        : this.Transform(expr);

    private static T KeepAttr<T>(T orig, T trafo)
        where T : Ast
    {
        trafo.Attributes = orig.Attributes;
        return trafo;
    }

    private static (ImmutableList<T> Targets, ImmutableList<Decl> Others) FilterSeq<T>(IEnumerable<Decl> decls)
        where T : Decl => decls
        .Select(FilterSeq<T>)
        .Aggregate((left, right) => (
            left.Targets.Concat(right.Targets).ToImmutableList(),
            left.Others.Concat(right.Others).ToImmutableList()));

    private static (ImmutableList<T> Targets, ImmutableList<Decl> Others) FilterSeq<T>(Decl decl)
        where T : Decl
    {
        var targets = ImmutableList.CreateBuilder<T>();
        var others = ImmutableList.CreateBuilder<Decl>();

        void Iterate(Decl d)
        {
            if (d is Decl.Seq s)
            {
                foreach (var i in s.Decls) Iterate(i);
            }
            else if (d is T t)
            {
                targets!.Add(t);
            }
            else
            {
                others!.Add(d);
            }
        }

        Iterate(decl);

        return (targets.ToImmutable(), others.ToImmutable());
    }
}
