module setup;

import
    std.stdio, std.file, std.process, std.path, std.string, std.getopt;

version(X86)    version(linux)  version = nux32;
version(X86_64) version(linux)  version = nux64;
version(X86)    version(Windows)version = win32;

version(Windows)
{
    enum exeExt = ".exe";
    pragma(lib, "ole32.lib");
}
else enum exeExt = "";

alias ImpType = immutable ubyte[];
alias ResType = immutable Resource;

enum Kind
{
    exe,
    dat,
    doc,
}

struct Resource
{
    @disable this(this);
    ImpType data;
    immutable string destName;
    immutable Kind kind;
}

immutable Resource[] ceResources =
[
    Resource(cast(ImpType) import("dexed" ~ exeExt), "dexed" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("dastworx" ~ exeExt), "dastworx" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("dexed.ico"), "dexed.ico", Kind.dat),
    Resource(cast(ImpType) import("dexed.png"), "dexed.png", Kind.dat),
    Resource(cast(ImpType) import("dexed.license.txt"), "dexed.license.txt", Kind.doc)
];

immutable Resource[] thirdPartBinaries =
[
    Resource(cast(ImpType) import("dcd-server" ~ exeExt), "dcd-server" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("dcd-client" ~ exeExt), "dcd-client" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("dscanner" ~ exeExt), "dscanner" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("dcd.license.txt"), "dcd.license.txt", Kind.doc)
];

immutable Resource[] oldResources =
[
    Resource(cast(ImpType) [], "cesyms" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) [], "cetodo" ~ exeExt, Kind.exe),
];

struct Formater
{
    private enum width = 54;
    private static __gshared char[] separator;
    
    static this()
    {
        separator.length = width + 4;
        separator[] =  '-';
        separator[0] = '+';
        separator[$-1] = '+';
    }
    
    static void justify(char A)(string s)
    in
    {
        assert (s.length <= width, "too long to fit on a line...");
    }
    body
    {
        static if (A == 'L') 
            writeln("| ",  leftJustify(s, width, ' '), " |");
        else static if (A == 'C') 
            writeln("| ",  center(s, width), " |");
        else static if (A == 'R') 
            writeln("| ",  rightJustify(s, width, ' '), " |");  
        else static assert(0, "invalid justification, L|C|R expected");      
    }  
    
    static void separate(){separator.writeln;}
    
    static void emptyLine(){justify!'L'("");}
}

static immutable string exePath, datPath, shortCutPath;
version(linux) immutable bool asSu;

static this()
{
    version(Windows)
    { 
        exePath = environment.get("PROGRAMFILES") ~ r"\dexed\";
        datPath = environment.get("APPDATA") ~ r"\dexed\";
        shortCutPath = environment.get("USERPROFILE") ~ r"\Desktop\";
    }
    else
    {
        asSu = environment.get("SUDO_USER") != "";
        if (asSu)
        {
            exePath = "/usr/bin/";
            datPath = "/home/" ~ environment.get("SUDO_USER") ~ "/.config/dexed/";
            shortCutPath = "/usr/share/applications/";
        }
        else
        {
            exePath = "/home/" ~ environment.get("USER") ~ "/bin/";
            datPath = "/home/" ~ environment.get("USER") ~ "/.config/dexed/";
            shortCutPath = "/home/" ~ environment.get("USER") ~ "/.local/share/applications/";
        }
    }
} 

void main(string[] args)
{
    bool noTools;
    bool uninstall;
    bool listfiles;

    getopt(args, config.passThrough, 
        "nodcd", &noTools,
        "notools", &noTools,
        "u|uninstall", &uninstall,
        "l|list", &listfiles
    );
    
    Formater.separate;

    if (listfiles)
    {
        static immutable fmtRes = "\"%s\" installed: %s";
        static immutable fmtOldRes = "obsolete \"%s\" installed: %s";
        string fname;

        Formater.separate;
        Formater.justify!'C'("files list and status");
        Formater.separate;

        foreach (ref res; ceResources)
        {
            fname = targetFilename(res);
            writefln(fmtRes, fname, exists(fname));
        }
        foreach (ref res; thirdPartBinaries)
        {
            fname = targetFilename(res);
            writefln(fmtRes, fname, exists(fname));
        }
        foreach (ref res; oldResources)
        {
            fname = targetFilename(res);
            writefln(fmtOldRes, fname, exists(fname));
        }

        Formater.separate;
        return;
    }

    if (!uninstall) Formater.justify!'C'(format("dexed %s - setup",
        import("version.txt")[1..$].chomp));
    else Formater.justify!'C'("dexed uninstaller");
    
    Formater.separate;
    version(Windows) Formater.justify!'L'("the setup program must be run as admin");
    else 
    {   
        if(!asSu) Formater.justify!'L'("dexed will be accessible to the current user");
        else Formater.justify!'L'("dexed will be accessible to all the users");
    }
    
    Formater.separate;
    Formater.justify!'L'("options:");
    Formater.emptyLine;
    Formater.justify!'L'("-l | --list: list files and status");
    if (!uninstall) 
    {
        Formater.justify!'L'("-u | --uninstall: uninstall");
        if (!noTools) Formater.justify!'L'("--notools: skip DCD and Dscanner setup");
    }
    else if (!noTools) Formater.justify!'L'("--notools: do not remove DCD and Dscanner");
    Formater.justify!'L'("press A to abort or another key to start...");
    Formater.separate;   
    
    const string inp = readln.strip;
    if (inp.toLower == "a") return;
    
    Formater.separate;

    size_t failures;
    bool done;
    if(!uninstall)
    {
        static immutable extractMsg = [": FAILURE", ": extracted"];
        static immutable oldMsg = [": FAILURE", ": removed old file"];
        foreach (ref res; ceResources)
        {
            done = installResource(res);
            Formater.justify!'L'(res.destName ~ extractMsg[done]);
            failures += !done;
        }
        foreach (ref res; oldResources)
        {
            if (!res.targetFilename.exists)
                continue;
            done = uninstallResource(res);
            Formater.justify!'L'(res.destName ~ oldMsg[done]);
            failures += !done;
        }
        if (!noTools) foreach (ref res; thirdPartBinaries)
        {
            done = installResource(res);
            Formater.justify!'L'(res.destName ~ extractMsg[done]);
            failures += !done;
        }
        Formater.separate;
        if (failures)
            Formater.justify!'L'("there are ERRORS, plz contact the support");
        else
        {
            postInstall();
            Formater.justify!'L'("the files are correctly extracted...");
        }
    }
    else
    {
        // check that uninstall is executed as install (sudo or not)
        version(linux)
        {
            if (!asSu && exists("/usr/bin/dexed"))
            {
                Formater.separate;
                Formater.justify!'L'("warning, CE seems to be installed with sudo");
                Formater.justify!'L'("but the uninstaller is not launched with sudo.");
                Formater.separate;
            }
            else if (asSu && exists("/home/" ~ environment.get("USER") ~ "/bin/dexed"))
            {
                Formater.separate;
                Formater.justify!'L'("warning, CE seems not to be installed with sudo");
                Formater.justify!'L'("...but the uninstaller is launched with sudo.");
                Formater.separate;
            }
        }
        // uninstall
        static immutable rmMsg = [": FAILURE", ": deleted"];
        foreach (ref res; ceResources)
        {
            done = uninstallResource(res);
            Formater.justify!'L'(res.destName ~ rmMsg[done]);
            failures += !done;
        }
        if (!noTools) foreach (ref res; thirdPartBinaries)
        {
            done = uninstallResource(res);
            Formater.justify!'L'(res.destName ~ rmMsg[done]);
            failures += !done;
        }
        foreach (ref res; oldResources)
        {
            if (!res.targetFilename.exists)
                continue;
            done = uninstallResource(res);
            Formater.justify!'L'(res.destName ~ rmMsg[done]);
            failures += !done;
        }
        // remove $PF folder
        version(Windows)
        {
            try
                rmdir(exePath);
            catch(FileException e)
                failures++;
        }

        Formater.separate;
        if (failures)
            Formater.justify!'L'("there are ERRORS, plz contact the support");
        else
        {
            postUninstall();
            Formater.justify!'L'("the files are correctly removed...");
        }
    }
    Formater.emptyLine;
    Formater.justify!'R'("...press a key to exit");
    Formater.separate;
    readln;
}

/// Returns the resource target filename, according to its Kind
string targetFilename(ref ResType resource)
{
    with(Kind) final switch(resource.kind)
    {
        case Kind.exe: return exePath ~ resource.destName;
        case Kind.dat: return datPath ~ resource.destName;
        case Kind.doc: return datPath ~ resource.destName;
    }
}

/// Extracts and writes a resource to a file.
bool installResource(ref ResType resource)
{
    const string fname = resource.targetFilename;
    const string path = fname.dirName;
    if (!path.exists)
        mkdirRecurse(path);
    if (!path.exists)
        return false;
    
    try 
    {
        File f = File(resource.targetFilename, "w");
        f.rawWrite(resource.data);
        f.close;
        
        version(linux) if (resource.kind == Kind.exe && fname.exists)
            executeShell("chmod a+x " ~ fname);
    } 
    catch (Exception e) 
        return false;
    
    return true;
}

/// Deletes the file created for a resource
bool uninstallResource(ref ResType resource)
{
    const string fname = resource.targetFilename;
    if (!fname.exists)
        return true;
    else
        return tryRemove(fname);
}

/// returns true if fname is deleted
bool tryRemove(string fname)
{
    bool result = true;
    try
        remove(fname);
    catch (FileException e)
        result = false;
    return result;  
}

/// adds menu entry, shortcut, etc
void postInstall()
{
    version(Windows)
    {
        import
            core.sys.windows.basetyps, core.sys.windows.com,
            core.sys.windows.objbase, core.sys.windows.objidl,
            core.sys.windows.shlobj, core.sys.windows.windef,
            std.utf;

        extern(C) const GUID CLSID_ShellLink     = {0x00021401, 0x0000, 0x0000,
          [0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46]};

        extern(C) const IID IID_IShellLinkA      = {0x000214EE, 0x0000, 0x0000,
          [0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46]};

        extern(C) const IID IID_IPersistFile     = {0x0000010B, 0x0000, 0x0000,
          [0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46]};

        char[MAX_PATH] _desktopFolder;
        SHGetFolderPathA(null, CSIDL_DESKTOPDIRECTORY, null, 0, _desktopFolder.ptr);
        char[] desktopFolder = _desktopFolder.ptr.fromStringz();
        string target = exePath ~ "dexed.exe";
        string wdir = exePath ~ "";
        const(wchar)* linkPath = buildNormalizedPath(desktopFolder, "dexed.lnk").toUTF16z();

        CoInitialize(null);
        IShellLinkA shellLink;
        IPersistFile linkFile;
        CoCreateInstance(&CLSID_ShellLink, null, CLSCTX_INPROC_SERVER,
            &IID_IShellLinkA, cast(void**)&shellLink);
        shellLink.SetIconLocation(buildNormalizedPath(datPath, "dexed.ico").toStringz, 0);
        shellLink.SetPath(target.ptr);
        shellLink.SetWorkingDirectory(wdir.ptr);
        shellLink.QueryInterface(&IID_IPersistFile, cast(void**)&linkFile);
        linkFile.Save(linkPath, TRUE);
        CoUninitialize();
    }
    else version(linux)
    {
        mkdirRecurse(shortCutPath);
        File f = File(shortCutPath ~ "dexed.desktop", "w");
        f.writeln("[Desktop Entry]");
        f.writeln("Name=dexed");
        f.writeln("Path=" ~ exePath);
        f.writeln("Exec=" ~ exePath ~ "dexed %f");
        f.writeln("Icon=" ~ datPath ~ "dexed.png");
        f.writeln("Type=Application");
        f.writeln("Categories=Application;IDE;Development;");
        f.writeln("Keywords=editor;Dlang;IDE;dmd;");
        f.writeln("StartupNotify=true");
        f.writeln("Terminal=false");
        f.close;
    }
}

/// removes menu entry shortcuts, etc
void postUninstall()
{
    version(Windows)
    {
        tryRemove(shortCutPath ~ "dexed.lnk");
    }
    else version(linux)
    {
        tryRemove(shortCutPath ~ "dexed.desktop");
    }
}

