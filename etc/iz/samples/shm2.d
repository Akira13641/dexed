#!dmd -g -gs
/+ dub.sdl:
name "shm2"
dependency "iz" path= "../"
outputPath "./"
+/
module shm2;

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
alias Client = SimpleIpcProtocolClient!CommandKind;

void main()
{
    Client client;
    bool next = true;
    size_t cnt;

    void sendKey(Sip.Command* c, Sip.Data* d)
    {
        writeln("processed command ", cnt, ": ", d.merge!char());
        c.commandStatus = CommandStatus.processed;
        c.dataIndex = -1;
        next = true;
    }

    client.setCommandHandler(CommandKind.sendKey, &sendKey);
    client.startListening("some_ident", 13, 4096 * 128);

    while (client.running())
    {
        if (next)
        {
            // issue #15: server doesn't clean processed items if client doesn't sleep
            Thread.sleep(dur!"usecs"(100));

            if (cnt++ == 1024 * 1024)
                client.stopServer();
            else
                client.send(CommandKind.sendKey, []);
            next = false;
        }
    }
}
