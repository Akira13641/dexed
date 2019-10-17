#!runnable-flags: -O -release -inline -boundscheck=off

/+dub.sdl:
name "dictionnary_suffixarray"
dependency "iz" path="../"
+/

module dictionnary_suffixarray;

import std.stdio, std.file;
import iz.strings;

enum Mode
{
    match,
    proposal
}

void help()
{
    writeln("commands");
    writeln("'H'  : display this message");
    writeln("'MP' : switch to proposals (default mode)");
    writeln("'MM' : switch to match");
    writeln("'MEM': display memory usage in byte");
    writeln("'q'  : quit");
    writeln(`\q'  : escape quit command`);
}

void main(string[] args)
{
    version(Posix) if (args.length == 1 && "/usr/share/dict/words".exists)
        args ~= "/usr/share/dict/words";

    char[][] words;
    if (args.length > 1 && args[1].exists)
    {
        File f = File(args[1], "r");
        foreach(line; f.byLine)
            words ~= line.dup;
        f.close;
    }
    else return;

    SuffixArray!(char[]) arr = SuffixArray!(char[])(words);
    words.length = 0;
    Mode mode = Mode.proposal;

    writeln("dictionnary is ready...");
    help();
    stdout.flush;

    while (true)
    {
        string q = readln[0..$-1];
        if (q == q.stringof)
        {
            writeln("quiting...");
            stdout.flush;
            break;
        }
        else if (q == "MP")
            mode = Mode.proposal;
        else if (q == "MM")
            mode = Mode.match;
        else if (q == "CL")
            arr.clear;
        else if (q == "MEM")
            writeln(arr.memoryUsage);
        else if (q == "HELP" || q == "H")
            help();
        else if (!q.length)
            writeln("not a word...");
        else
        {
            if (q[0] == '\\')
                q = q[1..$];
            with (Mode) final switch (mode)
            {
                case match:
                    writeln(q, " - " , q in arr);
                    break;
                case proposal:
                    if (auto n = arr.findPrefix(q))
                        writeln(q, " + ", n.entries);
                    else writeln("prefix not found...");
            }
        }
        stdout.flush;
    }
}
