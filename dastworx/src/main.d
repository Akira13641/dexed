module dastworx;

import
    core.memory;
import
    std.array, std.getopt, std.stdio, std.path, std.algorithm, std.functional,
    std.file;
import
    iz.memory: construct;
import
    iz.options: Argument, ArgFlags, ArgFlag, handleArguments, CantThrow;
import
    dparse.lexer, dparse.parser, dparse.ast, dparse.rollback_allocator;
import
    common, todos, symlist, imports, mainfun, halstead, ddoc_template;


void main(string[] args)
{
    foreach(ref buffer; stdin.byChunk(4096))
        Launcher.source.put(buffer);
    handleArguments!(CantThrow, Launcher)(args[1..$]);
}

struct Launcher
{
    static this()
    {
        GC.disable;
        source.reserve(1024^^2);
    }

    __gshared @Argument("-l") int caretLine;
    __gshared @Argument("-o") bool option1;

    __gshared Appender!(ubyte[]) source;
    __gshared string[] files;

    // -o : deep visit the symbols
    // alias deepSymList = option1;
    // -o : outputs /++ +/ ddoc instead of /** */
    // alias plusComment = option1;

    /// Writes the list of files to process
    @Argument("-f")
    static void setFiles(string value)
    {
        files = value
            .splitter(pathSeparator)
            .filter!exists
            .array;
    }

    /// Writes the symbol list
    @Argument("-s", "", ArgFlags(ArgFlag.stopper))
    static void handleSymListOption()
    {
        mixin(logCall);

        static Appender!(AstErrors) errors;

        static void handleErrors(string fname, size_t line, size_t col, string message, bool err)
        {
            errors ~= construct!(AstError)(cast(ErrorType) err, message, line, col);
        }

        RollbackAllocator alloc;
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        LexerConfig config = LexerConfig("", StringBehavior.source);

        source.data
            .getTokensForParser(config, &cache)
            .parseModule("", &alloc, &handleErrors)
            .listSymbols(errors.data, option1);
    }

    /// Writes the list of todo comments
    @Argument("-t", "", ArgFlags(ArgFlag.stopper))
    static void handleTodosOption()
    {
        mixin(logCall);
        if (files.length)
            getTodos(files);
    }

    /// Writes the import list
    @Argument("-i", "", ArgFlags(ArgFlag.stopper))
    static void handleImportsOption()
    {
        mixin(logCall);
        if (files.length)
        {
            listFilesImports(files);
        }
        else
        {
            RollbackAllocator alloc;
            StringCache cache = StringCache(StringCache.defaultBucketCount);
            LexerConfig config = LexerConfig("", StringBehavior.source);

            source.data
                .getTokensForParser(config, &cache)
                .parseModule("", &alloc, toDelegate(&ignoreErrors))
                .listImports();
        }
    }

    /// Writes if a main() is present in the module
    @Argument("-m", "", ArgFlags(ArgFlag.stopper))
    static void handleMainfunOption()
    {
        mixin(logCall);

        RollbackAllocator alloc;
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        LexerConfig config = LexerConfig("", StringBehavior.source);

        source.data
            .getTokensForParser(config, &cache)
            .parseModule("", &alloc, toDelegate(&ignoreErrors))
            .detectMainFun();
    }

    /// Writes the halstead metrics
    @Argument("-H", "", ArgFlags(ArgFlag.stopper))
    static void handleHalsteadOption()
    {
        mixin(logCall);

        RollbackAllocator alloc;
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        LexerConfig config = LexerConfig("", StringBehavior.source);

        source.data
            .getTokensForParser(config, &cache)
            .parseModule("", &alloc, toDelegate(&ignoreErrors))
            .performHalsteadMetrics;
    }

    /// Writes the ddoc template for a given declaration
    @Argument("-K", "", ArgFlags(ArgFlag.stopper))
    static void handleDdocTemplateOption()
    {
        mixin(logCall);

        RollbackAllocator alloc;
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        LexerConfig config = LexerConfig("", StringBehavior.source);

        source.data
            .getTokensForParser(config, &cache)
            .parseModule("", &alloc, toDelegate(&ignoreErrors))
            .getDdocTemplate(caretLine, option1);
    }
}

