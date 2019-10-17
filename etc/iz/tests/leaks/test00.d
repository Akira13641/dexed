module test00;

void main()
{
    import iz.streams, iz.memory;
    MemoryStream str = construct!MemoryStream;
    str.size = 8192;
    str.size = str.size * 4;
    str.size = 8192;
    str.size = str.size * 4;
    destruct(str);
}
