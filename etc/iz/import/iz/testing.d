/**
 * Experimental custom unittest runner.
 *
 * It's not part of iz but it's used to test the library.
 */
module iz.testing;

import
    std.traits, std.typecons;

pragma(msg, "testing using " ~ __VENDOR__ ~ " version " ~ __VERSION__.stringof ~ "\n");

alias TestProto = void function();
alias TestBattery = TestProto[];
alias OwnedTestBattery = Tuple!(string, TestBattery);

enum UnitTest;

/**
 * Returns the unittests located in parent.
 *
 * Params:
 *      parent = A module, a class, a struct, ... that contains the tests.
 *      filter = A template that accepts a TestProto and that evaluates to a bool.
 *
 * Returns:
 *      A tuple made of a string that represents the test owner and an array of
 *      void function() that represents the tests located in the parent.
 */
OwnedTestBattery collectTest(alias parent, alias filter = null)()
{
    TestBattery result;
    foreach(test; __traits(getUnitTests, parent))
    {
        static if (is(typeof(filter!test)))
        {
            static if (filter!test)
                result ~= &test;
        }
        else result ~= &test;
    }
    return tuple(parent.stringof, result);
}

/**
 * Example of a template that can be used as filter in collectTest.
 */
template selectTestWithUda(alias T)
{
    enum selectTestWithUda = hasUDA!(T, UnitTest);
}

/**
 * Runs a test battery, printing the progress to stdout.
 *
 * Params:
 *      t = an OwnedTestBattery.
 *
 * Returns:
 *      A boolean indicating if the tests are all passed.
 */
bool runTestBattery(T, bool stopOnFailure = true)(T t)
{
    import std.stdio: writeln, stderr, stdout;
    import core.exception: AssertError;

    bool result = true;
    static if (is(T == OwnedTestBattery))
    {
        writeln("running tests for: ", t[0]);
        foreach(i, test; t[1])
        {
            try
            {
                test();
            }
            catch (AssertError err)
            {
                result = false;
                writeln("test ", i, ": failed");
                writeln(err.file, "(", err.line, "):", err.msg);
                static if (stopOnFailure)
                {
                    throw err;
                }
                else 
                    continue;
            }
            writeln("test ", i, ": passed");
            stdout.flush;
        }
    }
    else static assert(0, T.stringof ~ " not handled by " ~ __FUNCTION__);
    return result;
}

/**
 * Generates a string that represents the code to run the tests in a list of module.
 *
 * Params:
 *      Modules = A string that represents the list of the modules to test.
 */
string libraryTestCode(string Modules, bool stopOnFailure = true)()
{
    return
    "
        static this()
        {
            import core.runtime;
            core.runtime.Runtime.moduleUnitTester = &testModules!("
                ~ stopOnFailure.stringof ~ "," ~ Modules ~ ");
        }

        void main() {}
    ";
}

/// libraryTestCode() routine
bool testModules(bool stopOnFailure, Modules...)()
{
    import std.stdio: writeln;

    bool result = true;
    foreach(m; Modules)
    {
        auto tests = collectTest!(m)();
        if (!tests.runTestBattery)
        {
            result = false;
            static if (stopOnFailure)
                break;            
        }            
    }
    if (result)
        writeln("All the tests passed");

    return result;
}

