---
title: Widgets - application options
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

### File menu - reference

- **New empty module**: Creates a new empty module.
- **New runnable module**: Creates a new module with a `main()` function. See [runnables modules](features_runnables).
- **New module from clipboard**: Creates a new module, immediatly filled with the clipboard content.
- **New DUB script**: Creates a new module with a `main()` function and an embedded DUB receipt. The module can be executed as a script with **Run DUB single file package** or **Run DUB single file package outside**.
- **Open file...**: Proposes to open a D source file from an open dialog.
- **Open recent**: Displays the list of the most recently opened files.
- **Close file**: Closes the current editor with an optional warning if its content is modified.
- **Close all the files**: Closes the current editors.
- **Close all the other files**: Closes the current editors expected the one that has the focus.
- **Save file**: Saves the current editor to the disk.
- **Save all**: Saves all the files currently opened.
- **Save file as...**: Proposes to save the current editor from a save dialog.
- **Export html...**: Exports the current editor to a html file with html-based highlighting.
- **Add file to project**: Adds the file matching to the current editor to the current project (CE format only).
- **Compile file**: Compiles the current file. See [runnables modules](features_runnables).
- **Compile file and run**: Compiles the current file and executes it. See [runnables modules](features_runnables).
- **Compile file and run outside**: Compiles the current file and executes without redirection. See [runnables modules](features_runnables).
- **Compile file and run...**: Compiles the current file and execute with a set of options defined in a simple input-query dialog. The arguments must be separated with spaces and can include double quotes.
- **Run compiled file**: Executes the binary produced by a previous call to **Compile file**.
- **Run compiled file outside**: Executes in a console the binary produced by a previous call to **Compile file**.
- **Run file unittests**: Compiles and runs the current file with the options **-unittest**. If the application option **coverModuleTests** is set then CE will also pass **-cov** to the compiler and reports the lines of code that are not covered by the unit tests in the messages.
- **Set runnable switches**: Allows to modify the switches passed to DMD when compiling a runnable module. One switch by line, invalid, reserved or duplicated switches are removed, for example **-main** and **-unittest** are reserved.
- **Run DUB single file package**: Compiles and execute a DUB script.
- **Run DUB single file package outside**: Compiles and execute a DUB script in a console.
- **Verify with Dscanner**: verifies the current source with [Dscanner](https://github.com/Hackerpilot/Dscanner). Results are displayed in the [messages](widgets_messages).
- **View Halstead metrics**: Checks the halstead metrics in the source that has the focus and reports abnormal functions in the [messages](widgets_messages). [Description of the metric](options_code_metrics).
- **View in mini explorer**: expands the [mini explorer](widgets_mini_explorer) tree on the folder that contains the current file.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
