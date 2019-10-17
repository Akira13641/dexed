module ddoc_template;

import
    std.stdio;
import
    iz.memory, iz.sugar;
import
    dparse.ast, dparse.lexer, dparse.parser, dparse.rollback_allocator;

/**
 * Finds the declaration at caretLine and write its ddoc template
 * in the standard output.
 */
void getDdocTemplate(const(Module) mod, int caretLine, bool plusComment)
{
    DDocTemplateGenerator dtg = construct!DDocTemplateGenerator(caretLine, plusComment);
    dtg.visit(mod);
}

final class DDocTemplateGenerator: ASTVisitor
{
    alias visit = ASTVisitor.visit;

private:

    immutable int _caretline;
    immutable char c1;
    immutable char[2] c2;
    bool _throws;

public:

    this(int caretline, bool plusComment)
    {
        _caretline = caretline;
        c1 = plusComment ? '+' : '*';
        c2 = plusComment ? "++" : "**";
    }

    override void visit(const(ThrowStatement) ts)
    {
        _throws = true;
    }

    override void visit(const(Catch) c)
    {
        _throws = false;
    }

    override void visit(const(FunctionDeclaration) decl)
    {
        _throws = false;
        if (decl.name.line == _caretline)
        {
            decl.accept(this);
            writeln("/", c2, "\n ", c1, " <short description> \n ", c1, " \n ", c1, " <detailed description>", c1);

            const TemplateParameterList tpl = safeAccess(decl).templateParameters.templateParameterList;
            if ((tpl && tpl.items.length) ||
                (decl.parameters && decl.parameters.parameters.length))
            {
                writeln(" ", c1, " \n ", c1, " Params:");

                if (tpl)
                {
                    foreach(const TemplateParameter p;  tpl.items)
                    {
                        if (p.templateAliasParameter)
                            writeln(" ", c1, "     ", p.templateAliasParameter.identifier.text, " = <description>");
                        else if (p.templateTupleParameter)
                            writeln(" ", c1, "     ", p.templateTupleParameter.identifier.text, " = <description>");
                        else if (p.templateTypeParameter)
                            writeln(" ", c1, "     ", p.templateTypeParameter.identifier.text, " = <description>");
                        else if (p.templateValueParameter)
                            writeln(" ", c1, "     ", p.templateValueParameter.identifier.text, " = <description>");
                    }
                }
                if (decl.parameters)
                {
                    foreach(i, const Parameter p; decl.parameters.parameters)
                    {
                        if (p.name.text != "")
                            writeln(" ", c1, "     ", p.name.text, " = <description>");
                        else
                            writeln(" ", c1, "     __param", i, " = <description>");
                    }
                }
            }

            if (const Type2 tp2 = safeAccess(decl).returnType.type2)
            {
                if (tp2.builtinType != tok!"void")
                    writeln(" ", c1, " \n ", c1, " Returns: <return description>");
            }

            if (_throws)
            {
                writeln(" ", c1, " \n ", c1, " Throws: <exception type as hint for catch>");
            }

            writeln(" ", c1, "/");

        }
        else if (decl.name.line > _caretline)
            return;
    }

    override void visit(const(TemplateDeclaration) decl)
    {
        visitTemplateOrAggregate(decl);
    }

    override void visit(const(ClassDeclaration) decl)
    {
        visitTemplateOrAggregate(decl);
    }

    override void visit(const(StructDeclaration) decl)
    {
        visitTemplateOrAggregate(decl);
    }

    override void visit(const(UnionDeclaration) decl)
    {
        visitTemplateOrAggregate(decl);
    }

    override void visit(const(AutoDeclarationPart) decl)
    {
        if (decl.templateParameters)
            visitTemplateOrAggregate(decl);
    }

    private void visitTemplateOrAggregate(T)(const(T) decl)
    {
        size_t line;
        static if (__traits(hasMember, T, "name"))
            line = decl.name.line;
        else
            line = decl.identifier.line;

        if (_caretline == line)
        {
            writeln("/", c2, "\n ", c1, " <short description> \n ", c1, " \n ", c1, " <detailed description>", c1);

            const TemplateParameterList tpl = safeAccess(decl).templateParameters.templateParameterList;
            if (tpl && tpl.items.length)
            {
                writeln(" ", c1, " \n ", c1, " Params:");

                foreach(const TemplateParameter p;  tpl.items)
                {
                    if (p.templateAliasParameter)
                        writeln(" ", c1, "     ", p.templateAliasParameter.identifier.text, " = <description>");
                    else if (p.templateTupleParameter)
                        writeln(" ", c1, "     ", p.templateTupleParameter.identifier.text, " = <description>");
                    else if (p.templateTypeParameter)
                        writeln(" ", c1, "     ", p.templateTypeParameter.identifier.text, " = <description>");
                    else if (p.templateValueParameter)
                        writeln(" ", c1, "     ", p.templateValueParameter.identifier.text, " = <description>");
                }
            }
            writeln(" ", c1, "/");

        }
        else if (line > _caretline)
            return;
        decl.accept(this);
    }
}

version(unittest)
{
    DDocTemplateGenerator parseAndVisit(const(char)[] source, int caretLine, bool p = false)
    {
        writeln;
        RollbackAllocator allocator;
        LexerConfig config = LexerConfig("", StringBehavior.source, WhitespaceBehavior.skip);
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        const(Token)[] tokens = getTokensForParser(cast(ubyte[]) source, config, &cache);
        Module mod = parseModule(tokens, "", &allocator);
        DDocTemplateGenerator result = construct!(DDocTemplateGenerator)(caretLine, p);
        result.visit(mod);
        return result;
    }
}

unittest
{
    q{ module a;
       void foo(A...)(A a){}
    }.parseAndVisit(2, true);
}

unittest
{
    q{ module a;
       void foo()(){}
    }.parseAndVisit(2);
}

unittest
{
    q{ module a;
       int foo(int){}
    }.parseAndVisit(2, true);
}

unittest
{
    q{ module a;
       class Foo(T, A...){}
    }.parseAndVisit(2);
}

unittest
{
    q{ module a;
       struct Foo(alias Fun, A...){}
    }.parseAndVisit(2);
}

unittest
{
    q{ module a;
       enum trait(alias Variable) = whatever;
    }.parseAndVisit(2);
}

