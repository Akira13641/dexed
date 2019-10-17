module mainfun;

import
    std.stdio, std.algorithm;
import
    iz.memory, iz.sugar;
import
    dparse.lexer, dparse.ast, dparse.parser;
import
    common;

/**
 * Detects wether a main function is declared in a module.
 *
 * Writes "1" if a main is found otherwise "0". The detection is not accurate,
 * if the main is injected by a mixin template or by a string it is not detected,
 * if the main is deactivated by a static condition neither.
 *
 * The result is used to determine if the "-main" switch has to be passed to
 * the compiler when a runnable module is executed or a module tested.
 */
void detectMainFun(const(Module) mod)
{
    mixin(logCall);
    MainFunctionDetector mfd = construct!(MainFunctionDetector);
    mfd.visit(mod);
    write(mfd.hasMain);
}

private final class MainFunctionDetector: ASTVisitor
{
    alias visit = ASTVisitor.visit;

    ubyte hasMain;

    override void visit(const ConditionalDeclaration decl)
    {
        bool acc = true;
        if (const VersionCondition vc = safeAccess(decl).compileCondition.versionCondition)
        {
            if (vc.token.text in badVersions())
                acc = false;
        }
        if (acc)
            decl.accept(this);
    }

    override void visit(const(FunctionDeclaration) decl)
    {
        if (decl.name.text == "main")
            hasMain = true;
    }

    override void visit(const(Unittest)){}
    override void visit(const(ClassDeclaration)){}
    override void visit(const(StructDeclaration)){}
    override void visit(const(InterfaceDeclaration)){}
    override void visit(const(FunctionBody)){}
}

