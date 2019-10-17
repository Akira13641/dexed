module halstead;

import
    std.algorithm, std.conv, std.json, std.meta;
import
    std.stdio, std.ascii, std.digest.crc, std.range: iota;
import
    dparse.ast, dparse.lexer, dparse.parser, dparse.rollback_allocator;
import
    iz.memory, iz.containers, iz.sugar;
version(unittest){} else import
    common;

/**
 * Retrieves the count and unique count of the operands and operators of
 * each function (inc. methods) of a module. After the call the results are
 * serialized as JSON in te standard output.
 */
void performHalsteadMetrics(const(Module) mod)
{
    HalsteadMetric hm = construct!(HalsteadMetric);
    hm.visit(mod);
    hm.serialize;
}

private struct Function
{
    size_t line;
    string name;
    size_t N1, n1;
    size_t N2, n2;
    alias operatorsSum = N1;
    alias operatorsKinds = n1;
    alias operandsSum = N2;
    alias operandsKinds = n2;
}

private struct BinaryExprFlags
{
    bool leftIsFunction;
    bool rightIsFunction;
}

private final class HalsteadMetric: ASTVisitor
{
    alias visit = ASTVisitor.visit;

    Function[] functions;
    HashMap_AB!(string, size_t) operators;
    HashMap_AB!(string, size_t) operands;
    BinaryExprFlags[] binExprFlag;
    size_t functionNesting;
    bool[] inFunctionCallChain;
    const(IdentifierOrTemplateInstance)[] chain;
    bool ifStatement;
    JSONValue fs;

    void processCallChain()
    {
        version(none)
        {
            import std.array : join;
            writeln("chain: ", chain.map!(a => a.identifier.text).join("."));
        }
        if (chain.length)
        {
            static Token getIdent(const(IdentifierOrTemplateInstance) i)
            {
                if (i.identifier != tok!"")
                    return i.identifier;
                else
                    return i.templateInstance.identifier;
            }
            foreach(i, ident; chain)
            {
                if (i == chain.length-1)
                    operators[getIdent(ident).text] +=1;
                else
                    operands[getIdent(ident).text] +=1;
            }
            chain.length = 0;
        }
    }

    void addOperandFromToken(const ref Token tk)
    {
        if (isLiteral(tk.type))
        {
            alias immutHexStr = toHexString!(Order.increasing, LetterCase.upper);
            operands["literal" ~ immutHexStr(tk.text.crc32Of)] +=1;
        }
        else operands[tk.text] +=1;
    }

    void pushExprFlags(bool leftFlag = false, bool rightFlag = false)
    {
        binExprFlag.length += 1;
        binExprFlag[$-1].leftIsFunction = leftFlag;
        binExprFlag[$-1].rightIsFunction = rightFlag;
    }

    void popExprFlags()
    {
        binExprFlag.length -= 1;
    }

    bool exprLeftIsFunction(){return binExprFlag[$-1].leftIsFunction;}

    bool exprRightIsFunction(){return binExprFlag[$-1].rightIsFunction;}

    this()
    {
        fs = parseJSON("[]");
        pushExprFlags;
        inFunctionCallChain.length++;
    }

    void serialize()
    {
        JSONValue js;
        js["functions"] = fs;
        js.toString.write;
    }

    override void visit(const(PragmaExpression)){}
    override void visit(const(Unittest)){}

    void beginFunction()
    {
        operators.clear;
        operands.clear;
        operators.reserve(64);
        operands.reserve(64);
        if (functionNesting++ == 0)
            functions.length = functions.length + 1;
    }

    void endFunction(string name, size_t line)
    {
        functions[$-1].name = name;
        functions[$-1].line = line;

        if (operators.count)
        {
            functions[$-1].N1 = operators.byValue.fold!((a,b) => b = a + b);
            functions[$-1].n1 = operators.count;
        }
        if (operands.count)
        {
            functions[$-1].N2 = operands.byValue.fold!((a,b) => b = a + b);
            functions[$-1].n2 = operands.count;
        }

        JSONValue f;
        f["name"] = functions[$-1].name;
        f["line"] = functions[$-1].line;
        f["n1Sum"] = functions[$-1].N1;
        f["n1Count"] = functions[$-1].n1;
        f["n2Sum"] = functions[$-1].N2;
        f["n2Count"] = functions[$-1].n2;
        fs ~= [f];

        version(unittest)
        {
            writeln(functions[$-1]);
            writeln("\toperators: ",operators);
            writeln("\toperands : ",operands);
        }

        functionNesting--;
    }

    override void visit(const(TemplateArguments) ta)
    {
        ta.accept(this);
        if (ta.templateSingleArgument)
            addOperandFromToken(ta.templateSingleArgument.token);
    }

    override void visit(const(FunctionCallExpression) expr)
    {

        inFunctionCallChain.length++;
        inFunctionCallChain[$-1] = true;

        if (const TemplateSingleArgument tsi = safeAccess(expr)
            .templateArguments.templateSingleArgument)
                addOperandFromToken(tsi.token);

        expr.accept(this);

        if (inFunctionCallChain[$-1])
        {
            processCallChain;
        }

        inFunctionCallChain.length--;
    }

    override void visit(const(FunctionDeclaration) decl)
    {
        beginFunction;
        if (!decl.functionBody)
            return;
        decl.accept(this);
        endFunction(decl.name.text, decl.name.line);
    }

    void visitFunction(T)(const(T) decl)
    {
        beginFunction;
        decl.accept(this);
        endFunction(T.stringof ~ to!string(decl.line), decl.line);
    }

    static string funDeclString()
    {
        alias FunDecl = AliasSeq!(
            SharedStaticConstructor,
            StaticConstructor,
            Constructor,
            SharedStaticDestructor,
            StaticDestructor,
            Destructor,
            Postblit
        );

        string result;

        enum funDeclOverride(T) =
        "override void visit(const(" ~ T.stringof ~ ") decl)
        {
            visitFunction(decl);
        }";

        foreach(i; aliasSeqOf!(iota(0,FunDecl.length)))
        {
            result ~= funDeclOverride!(FunDecl[i]);
        }

        return result;
    }

    mixin(funDeclString);

    override void visit(const(PrimaryExpression) primary)
    {
	    if (primary.identifierOrTemplateInstance !is null)
        {
            if (inFunctionCallChain[$-1])
                chain ~= primary.identifierOrTemplateInstance;
            if ((!inFunctionCallChain[$-1]) ||
                (inFunctionCallChain[$-1] & exprLeftIsFunction) ||
                (inFunctionCallChain[$-1] & exprRightIsFunction))
            {
                operands[primary.identifierOrTemplateInstance.identifier.text] +=1;
            }
        }
        else addOperandFromToken(primary.primary);
        primary.accept(this);
    }

    override void visit(const(ArgumentList) al)
    {
        if (inFunctionCallChain[$-1])
            processCallChain;
        inFunctionCallChain[$-1] = false;
        al.accept(this);
    }

    override void visit(const(UnaryExpression) expr)
    {
        expr.accept(this);

        if (expr.identifierOrTemplateInstance)
        {
            operators["."] += 1;

            if (inFunctionCallChain[$-1])
                chain ~= expr.identifierOrTemplateInstance;
            else
            {
                if (expr.identifierOrTemplateInstance.identifier != tok!"")
                    operands[expr.identifierOrTemplateInstance.identifier.text] +=1;
                else
                    operands[expr.identifierOrTemplateInstance.templateInstance.identifier.text] +=1;
            }
        }

        if (expr.prefix.type)
            operators[str(expr.prefix.type)] += 1;
        if (expr.suffix.type)
            operators[str(expr.suffix.type)] += 1;
    }
    override void visit(const(AsmInstruction) ai)
    {
        if (ai.identifierOrIntegerOrOpcode != tok!"")
        {
            operators[ai.identifierOrIntegerOrOpcode.text] += 1;
        }
        ai.accept(this);
    }

    override void visit(const(Register) reg)
    {
        if (reg.identifier != tok!"")
        {
            operands[reg.identifier.text] +=1;
        }
        if (reg.hasIntegerLiteral)
        {
            addOperandFromToken(reg.intLiteral);
        }
        reg.accept(this);
    }

    override void visit(const(AsmPrimaryExp) ape)
    {
        if (ape.token != tok!"")
            addOperandFromToken(ape.token);
        if (ape.identifierChain)
            ape.identifierChain.identifiers
            .filter!(a => !a.text.among("dword","ptr"))
            .each!(a => addOperandFromToken(a));

        ape.accept(this);
    }

    override void visit(const(IndexExpression) expr)
    {
        operators["[]"] += 1;
        expr.accept(this);
    }

    override void visit(const(NewExpression) expr)
    {
        operators["new"] += 1;
        expr.accept(this);
    }

    override void visit(const(NewAnonClassExpression) expr)
    {
        operators["new"] += 1;
        expr.accept(this);
    }

    override void visit(const(DeleteExpression) expr)
    {
        operators["delete"] += 1;
        expr.accept(this);
    }

    override void visit(const(CastExpression) expr)
    {
        operators["cast"] += 1;
        expr.accept(this);
    }

    override void visit(const(IsExpression) expr)
    {
        operators["is"] += 1;
        expr.accept(this);
    }

    override void visit(const(TernaryExpression) expr)
    {
        if (expr.orOrExpression)
            operators["if"] += 1;
        if (expr.expression)
            operators["else"] += 1;
        expr.accept(this);
    }

    override void visit(const(TypeidExpression) expr)
    {
        operators["typeid"] += 1;
        expr.accept(this);
    }

    override void visit(const(IfStatement) st)
    {
        operators["if"] += 1;
        st.accept(this);
        if (st.thenStatement)
            operators["then"] += 1;
        if (st.elseStatement)
            operators["else"] += 1;
    }

    override void visit(const(WhileStatement) st)
    {
        operators["while"] +=1;
        st.accept(this);
    }

    override void visit(const(ForStatement) st)
    {
        operators["for"] +=1;
        st.accept(this);
    }

    override void visit(const(ForeachStatement) st)
    {
        operators["foreach"] +=1;
        if (st.foreachTypeList)
            foreach(ft; st.foreachTypeList.items)
                operands[ft.identifier.text] +=1;
        if (st.foreachType)
            operands[st.foreachType.identifier.text] +=1;
        st.accept(this);
    }

    override void visit(const(ReturnStatement) st)
    {
        operators["return"] +=1;
        st.accept(this);
    }

    override void visit(const(BreakStatement) st)
    {
        operators["break"] +=1;
        st.accept(this);
    }

    override void visit(const(ContinueStatement) st)
    {
        operators["continue"] +=1;
        st.accept(this);
    }

    override void visit(const(GotoStatement) st)
    {
        operators["goto"] +=1;
        st.accept(this);
    }

    override void visit(const(SwitchStatement) st)
    {
        operators["switch"] +=1;
        st.accept(this);
    }

    override void visit(const(CaseStatement) st)
    {
        operators["case"] +=1;
        st.accept(this);
    }

    override void visit(const(CaseRangeStatement) st)
    {
        operators["case"] +=2;
        st.accept(this);
    }

    override void visit(const(DefaultStatement) st)
    {
        operators["case"] +=1;
        st.accept(this);
    }

    override void visit(const(ThrowStatement) st)
    {
        operators["throw"] +=1;
        st.accept(this);
    }

    override void visit(const(TryStatement) st)
    {
        operators["try"] +=1;
        st.accept(this);
    }

    override void visit(const(Catch) c)
    {
        operators["catch"] +=1;
        c.accept(this);
        if (c.identifier.text)
            operands[c.identifier.text] +=1;
    }

    override void visit(const(VariableDeclaration) decl)
    {
        if (decl.declarators)
            foreach (elem; decl.declarators)
            {
                operands[elem.name.text] +=1;
                if (elem.initializer)
                    operators["="] +=1;
            }
        else if (decl.autoDeclaration)
            visit(decl.autoDeclaration);
        decl.accept(this);
    }

    override void visit(const AutoDeclarationPart decl)
    {
        operands[decl.identifier.text] +=1;
        operators["="] +=1;
        decl.accept(this);
    }

    void visitBinExpr(T)(const(T) expr)
    {
        bool leftArgIsFunctFlag;
        bool rightArgIsFunctFlag;
        static if (__traits(hasMember, T, "left"))
        {
            if (expr.left && (cast(UnaryExpression) expr.left) &&
                (cast(UnaryExpression) expr.left).functionCallExpression)
                    leftArgIsFunctFlag = true;
        }
        static if (__traits(hasMember, T, "right"))
        {
            if (expr.right && (cast(UnaryExpression) expr.right) &&
                (cast(UnaryExpression) expr.right).functionCallExpression)
                    rightArgIsFunctFlag = true;
        }

        string op;
        static if (__traits(hasMember, T, "operator"))
        {
            op = str(expr.operator);
        }
        else
        {
            static if (is(T == AndExpression)) op = `&`;
            else static if (is(T == AndAndExpression)) op = `&&`;
            else static if (is(T == AsmAndExp)) op = `&`;
            else static if (is(T == AsmLogAndExp)) op = "&&";
            else static if (is(T == AsmLogOrExp)) op = "||";
            else static if (is(T == AsmOrExp)) op = "|";
            else static if (is(T == AsmXorExp)) op = "|";
            else static if (is(T == IdentityExpression)) op = expr.negated ? "!is" : "is";
            else static if (is(T == InExpression)) op = expr.negated ? "!in" : "in";
            else static if (is(T == OrExpression)) op = `|`;
            else static if (is(T == OrOrExpression)) op = `||`;
            else static if (is(T == PowExpression)) op = `^^`;
            else static if (is(T == XorExpression)) op = `^`;
            else static assert(0, T.stringof);
        }
        operators[op] +=1;

        pushExprFlags(leftArgIsFunctFlag, rightArgIsFunctFlag);
        expr.accept(this);
        popExprFlags;
    }

    static string binExprsString()
    {
        alias SeqOfBinExpr = AliasSeq!(
            AddExpression,
            AndExpression,
            AndAndExpression,
            AsmAddExp,
            AsmAndExp,
            AsmEqualExp,
            AsmLogAndExp,
            AsmLogOrExp,
            AsmMulExp,
            AsmOrExp,
            AsmRelExp,
            AsmShiftExp,
            AsmXorExp,
            AssignExpression,
            EqualExpression,
            IdentityExpression,
            InExpression,
            MulExpression,
            OrExpression,
            OrOrExpression,
            PowExpression,
            RelExpression,
            ShiftExpression,
            XorExpression,
        );

        enum binExpOverride(T) =
        "override void visit(const(" ~ T.stringof ~ ") expr)
        {
            visitBinExpr(expr);
        }";

        string result;
        foreach(i; aliasSeqOf!(iota(0, SeqOfBinExpr.length)))
            result ~= binExpOverride!(SeqOfBinExpr[i]);
        return result;
    }

    mixin(binExprsString());
}

version(unittest)
{
    T parseAndVisit(T : ASTVisitor)(const(char)[] source)
    {
        RollbackAllocator allocator;
        LexerConfig config = LexerConfig("", StringBehavior.source, WhitespaceBehavior.skip);
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        const(Token)[] tokens = getTokensForParser(cast(ubyte[]) source, config, &cache);
        Module mod = parseModule(tokens, "", &allocator);
        T result = construct!(T);
        result.visit(mod);
        return result;
    }

    Function test(string source)
    {
        HalsteadMetric hm = parseAndVisit!(HalsteadMetric)(source);
        scope(exit) destruct(hm);
        return hm.functions[$-1];
    }
}

unittest
{
    Function r =
    q{
        void foo()
        {
            Object o = new Object;
        }
    }.test;
    assert(r.operandsKinds == 1);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            auto o = new Object;
        }
    }.test;
    assert(r.operandsKinds == 1);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            auto o = 1 + 2;
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            foo(bar,baz);
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            int i = foo(bar,baz) + foo(bar,baz);
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 3);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            bar!("lit")(a);
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            enum E{e0}
            E e;
            bar!(e,"lit")(baz(e));
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo();
    }.test;
    assert(r.operandsKinds == 0);
    assert(r.operatorsKinds == 0);
}

unittest
{
    Function r =
    q{
        shared static this()
        {
            int i = 0;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        shared static ~this()
        {
            int i = 0;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        static this()
        {
            int i = 0;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        static ~this()
        {
            int i = 0;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        class Foo
        {
            this()
            {
                int i = 0;
            }
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        class Foo
        {
            ~this()
            {
                int i = 0;
            }
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            i += a << b.c;
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 3);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            ++h;
            i--;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            ++i--;
        }
    }.test;
    assert(r.operandsKinds == 1);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            i = a | b & c && d || e^f + g^^h - a in b + a[0];
        }
    }.test;
    assert(r.operandsKinds == 10);
    assert(r.operatorsKinds == 11);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            Bar bar = new Bar;
            auto baz = cast(Baz) bar;
            delete bar;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            foreach(i,a;z){}
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            foreach(i; l..h){}
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            for(i = 0; i < len; i++){}
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            for(;;){continue;}
        }
    }.test;
    assert(r.operandsKinds == 0);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        int foo()
        {
            while(true) {return 0;}
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            switch(a)
            {
                default: break;
                case 1: return;
                case 2: .. case 8: ;
            }
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            try a();
            catch(Exception e)
                throw v;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            if (true) {} else {i = 0;}
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = true ? 0 : 1;
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 3);
}

version(none) unittest
{
    // TODO: detect function call w/o parens
    Function r =
    q{
        void foo()
        {
            bar;
        }
    }.test;
    assert(r.operandsKinds == 0);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = bar(b) + baz(z);
        }
    }.test;
    assert(r.operandsKinds == 5);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = bar(cat(0) - dog(1)) + baz(z);
        }
    }.test;
    assert(r.operandsKinds == 8);
    assert(r.operatorsKinds == 7);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = bar(cat(0) && dog(1)) | baz(z);
        }
    }.test;
    assert(r.operandsKinds == 8);
    assert(r.operatorsKinds == 7);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = bar(c)++;
        }
    }.test;
    // would be 3 by considering bar as an operand
    // but this is actually (almost always) invalid code.
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 3);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = !!!a;
        }
    }.test;
    assert(r.operandsKinds == 1);
    assert(r.operatorsKinds == 2);
    assert(r.operatorsSum == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = b[foo(a)];
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 3);
}

unittest
{
    Function r =
    q{
        this(){a = 0;}
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        static this(){a = 0;}
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        shared static this(){a = 0;}
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}


unittest
{
    Function r =
    q{
        ~this(){a = 0;}
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        static ~this(){a = 0;}
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        shared static ~this(){a = 0;}
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        struct S{this(this){a = 0;}}
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        struct S{@disable this(this);}
    }.test;
    assert(r.operandsKinds == 0);
    assert(r.operatorsKinds == 0);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a.b.c = d.e;
        }
    }.test;
    assert(r.operandsKinds == 5);
    assert(r.operatorsKinds == 2);
    assert(r.operatorsSum == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a.b.c(d.e);
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 2);
    assert(r.operatorsSum == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a.b.c.d(e.f());
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 3);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a.b!(8,9).c = f;
        }
    }.test;
    assert(r.operandsKinds == 6);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a.b!8.c = f;
        }
    }.test;
    assert(r.operandsKinds == 5);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            asm{xor EAX,ECX;}
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            asm{mov EAX, SS:CL;}
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            asm{mov EAX, a.b.c;}
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            asm{imul EAX, dword [EBP + EBX * 4 + 0x0FFFFFFD4];}
        }
    }.test;
    assert(r.operandsKinds == 5);
    assert(r.operatorsKinds == 3);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            asm{imul EAX, ECX, dword[ESP + 8];}
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 2);
}

