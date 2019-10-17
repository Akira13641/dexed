module imports;

import
    std.stdio, std.algorithm, std.array, std.file, std.functional;
import
    iz.memory;
import
    dparse.lexer, dparse.ast, dparse.parser, dparse.rollback_allocator;
import
    common;

/**
 * Lists the modules imported by a module
 *
 * On the first line writes the module name or # between double quotes then
 * each import is written in a new line. Import detection is not accurate,
 * the imports injected by a mixin template or by a string variable are not detected,
 * the imports deactivated by a static condition neither.
 *
 * The results are used by to detect which are the static libraries used by a
 * runnable module.
 */
void listImports(const(Module) mod)
in
{
    assert(mod);
}
body
{
    mixin(logCall);
    if (mod.moduleDeclaration)
        writeln('"', mod.moduleDeclaration.moduleName.identifiers
            .map!(a => a.text).join("."), '"');
    else
        writeln("\"#\"");
    construct!(ImportLister).visit(mod);
}

/**
 * Lists the modules imported by several modules
 *
 * The output consists of several consecutive lists, as formated for
 * listImports. When no moduleDeclaration is available, the first line of
 * a list matches the filename.
 *
 * The results are used by to detect which are the static libraries used by a
 * runnable module.
 */
void listFilesImports(string[] files)
{
    mixin(logCall);
    RollbackAllocator allocator;
    StringCache cache = StringCache(StringCache.defaultBucketCount);
    LexerConfig config = LexerConfig("", StringBehavior.source);
    ImportLister il = construct!(ImportLister);
    foreach(fname; files)
    {
        ubyte[] source = cast(ubyte[]) std.file.read(fname);
        Module mod = parseModule(getTokensForParser(source, config, &cache),
            fname, &allocator, toDelegate(&ignoreErrors));
        if (mod.moduleDeclaration)
            writeln('"', mod.moduleDeclaration.moduleName.identifiers
                .map!(a => a.text).join("."), '"');
        else
            writeln('"', fname, '"');
        il.visit(mod);
    }
}

private final class ImportLister: ASTVisitor
{
    alias visit = ASTVisitor.visit;
    size_t mixinDepth;

    override void visit(const(Module) mod)
    {
        mixinDepth = 0;
        mod.accept(this);
    }

    override void visit(const ConditionalDeclaration decl)
    {
        bool acc = true;
        if (decl.compileCondition)
        {
            const ver = decl.compileCondition.versionCondition;
            if (ver && ver.token.text in badVersions)
                acc = false;
        }
        if (acc)
            decl.accept(this);
    }

    override void visit(const(ImportDeclaration) decl)
    {
        foreach (const(SingleImport) si; decl.singleImports)
        {
            if (!si.identifierChain.identifiers.length)
                continue;
            si.identifierChain.identifiers.map!(a => a.text).join(".").writeln;
        }
        if (decl.importBindings) with (decl.importBindings.singleImport)
            identifierChain.identifiers.map!(a => a.text).join(".").writeln;
    }

    override void visit(const(MixinExpression) mix)
    {
        ++mixinDepth;
        mix.accept(this);
        --mixinDepth;
    }

    override void visit(const PrimaryExpression primary)
    {
        if (mixinDepth && primary.primary.type.isStringLiteral)
        {
            assert(primary.primary.text.length > 1);

            size_t startIndex = 1;
            startIndex += primary.primary.text[0] == 'q';
            parseAndVisit!(ImportLister)(primary.primary.text[startIndex..$-1]);
        }
        primary.accept(this);
    }
}

