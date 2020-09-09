Dexed, the _D Extended EDitor_, is an IDE for the [D programming language](https://dlang.org), its compilers, tools and libraries.

## Important note:

Basile B. has now re-uploaded Dexed [on Gitlab](https://gitlab.com/basile.b/dexed) and appears to have resumed regular development on it. Thus, my repository here is no longer necessary.

## Overview

- available for _Linux_ or _Windows_.
- supports all the D compilers (DMD-GDC-LDC).
- supports the DUB projects (JSON or SDL) and also its own project format.
- support the DUB scripts (aka _single file packages_) and its own script format (aka _runnable modules_).
- full [D Completion Daemon](https://github.com/dlang-community/DCD) integration (completion, ddoc display, call tips, jump to declaration, rename identifier).
- Dynamic [D-Scanner](https://github.com/dlang-community/D-Scanner) linting with results displayed in the editor gutter.
- single click to compile and to _unittest_ a module and optionally display tests coverage.
- advanced editor with D2 syntax highlighter, folds, regions, identifier markup, macros, sync-edit, etc.
- additional highlighters for C and C++ sources, based on the D color scheme, for other files a generic bicolor highlighter is used.
- edition helpers: comment blocks, local identifier renaming, brace auto-closing, ddoc templates, etc.
- Debugging with a GDB gui. (**linux only**)
- Integrated terminal emulator. (**linux** + **GTK2 widget set** only)
- Tree of symbols in the current module.
- static library manager that supports auto-registration from local DUB projects, from online DUB packages or from dexed custom project format.
- todo list based on the _todo comments_ located in a project or in the current source.
- user-defined tools powered by a string interpolation system.
- integrated file browser, dfmt interface, search & replace, etc.

## Project information

- :scroll: licensed under the terms of the Boost software license.

## Notes on building

This project is now specifically being developed using the trunk versions of both Free Pascal and Lazarus, and uses language features that will not compile using FPC 3.0.4.

If Lazarus complains that it cannot find the `LGenerics` package, it means that you have not opened and compiled `etc/LGenerics/lgenerics/LGenerics.lpk` at least once.
