#!dmd -g -gs
/+ dub.sdl:
name "shm1"
dependency "iz" path= "../"
outputPath "./"
+/
module shm1;

import
    core.thread;
import
    std.stdio, std.random, std.range, std.conv, std.file, std.path;
import
    iz.ipc;

enum CommandKind : ubyte
{
    sendKey = 2,
}

alias Sip = SimpleIpcProtocol!CommandKind;
alias Server = SimpleIpcProtocolServer!(CommandKind);

char[] gen()
{
    return generate!(() => uniform('a', 'z')).takeExactly(16).array;
}

void main(string[] args)
{
    Server server;

    void sendKey(Sip.Command* c, Sip.Data* d)
    {
        c.commandStatus = CommandStatus.answered;
        c.dataIndex = server.addData(gen());
    }

    server.setCommandHandler(CommandKind.sendKey, &sendKey);
    server.startListening("some_ident", 13, 4096 * 128);
}
