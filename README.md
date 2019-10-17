Dexed, the _D Extended EDitor_, is an IDE for the [D programming language](https://dlang.org), its compilers, tools and libraries.

[![](https://basile-z.github.io/dexed/img/coedit_kde4_thumb.png)](https://basile-z.github.io/dexed/img/coedit_kde4.png)

## Overview

- available for _Linux_ or _Windows_.
- supports all the D compilers (DMD-GDC-LDC).
- supports the DUB projects (JSON or SDL) and also its own project format.
- support the DUB scripts (aka _single file packages_) and its own script format (aka _runnable modules_).
- full [D Completion Daemon](https://github.com/dlang-community/DCD) integration (completion, ddoc display, call tips, jump to declaration, rename identifier).
- Dynamic [D-Scanner](https://github.com/dlang-community/D-Scanner) linting with results displayed in the editor gutter.
- single click to compile and to _unittest_ a module and optionally display tests coverage.
- advanced editor with D2 syntax highlighter, folds, regions, identifier markup, macros, sync-edit, etc.
- additonal highlithers for C and C++ sources, based on the D color scheme, for other files a generic bicolor highlighter is used.
- edition helpers: comment blocks, local identifier renaming, brace auto-closing, ddoc templates, etc.
- Debugging with a GDB gui. (**linux only**)
- Integrated terminal emulator. (**linux** + **GTK2 widget set** only)
- Tree of symbols in the current module.
- static library manager that supports auto-registration from local DUB projects, from online DUB packages or from dexed custom project format.
- todo list based on the _todo comments_ located in a project or in the current source.
- user-defined tools powered by a string interpolation system.
- integrated file browser, dfmt interface, search & replace, [discover more in the manual](https://basile-z.github.io/dexed/).

## Project information

- :bookmark: latest release: version 3.7.4, Tue 22 Jan 2019.
- :scroll: licensed under the terms of the Boost software license.
- :dollar: Development can be supported with [Paypal donations](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AQDJVC39PJF7J).

## Download version 3.7.4

Download the zipped binaries or the zip that contains the setup program for you platform:

- :package: [setup program for Linux 64 bit](https://github.com/Basile-z/dexed/releases/download/v3.7.4/dexed.3.7.4.linux64.setup.zip)
- :package: [binaries for Linux 64 bit](https://github.com/Basile-z/dexed/releases/download/v3.7.4/dexed.3.7.4.linux64.zip)
- :package: [setup program for Windows 64 bit](https://github.com/Basile-z/dexed/releases/download/v3.7.4/dexed.3.7.4.win64.setup.zip)
- :package: [binaries for Windows 64 bit](https://github.com/Basile-z/dexed/releases/download/v3.7.4/dexed.3.7.4.win64.zip)
- :package: [rpm for Linux 64 bit](https://github.com/Basile-z/dexed/releases/download/v3.7.4/dexed-3.7.4-0.x86_64.rpm)
- :package: [deb for Linux 64 bit](https://github.com/Basile-z/dexed/releases/download/v3.7.4/dexed-3.7.4.amd64.deb)

The _zip_ archives allow to move freely the files.
The _setup.zip_ archives contain a command line program that installs to predefined locations so that the software can be run without additional intervention.
The _deb_ and the _rpm_ packages are for those who prefer the official setup system of their linux systems.
FreeBSD (all archs), Linux (32 bit) and Windows (32 bit) versions must be [built manually](https://basile-z.github.io/dexed/build.html).

[**See this page**](https://basile-z.github.io/dexed/setup.html) for more information about the setup.

## Building

Follow the procedure described [**here**](https://basile-z.github.io/dexed/build.html).
