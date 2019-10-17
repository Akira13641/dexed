---
title: Projects
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

### Projects

Two project formats are supported. 
For all the formats a single widget is used to display the source files list and the configurations, the [project inspector](widgets_project_inspector).
Project properties are edited in specific widgets.

#### DUB

Dexed handles [DUB](http://code.dlang.org/getting_started) projects.

DUB [JSON format](http://code.dlang.org/package-format?lang=json) format is fully supported.
DUB [SDL](http://code.dlang.org/package-format?lang=sdl) format is supported in read-only mode.

The widget used to edit the properties is the [DUB project editor](widgets_dub_project_editor).

The option editor exposes a [category for DUB](options_dub_build). The options specified in this category are applied each time a project is compiled.

#### DEXED format

The DEXED format (previously called _native format_ then _ce projects_) is based on DMD command line interface.
The widget used to edit the properties is the [DEXED project editor](widgets_dexed_project_editor) (the format is detailed in this page).

#### Menu reference

- **"New project"**: Closes and creates a new project (either with the native format or a DUB project). A warning may be displayed if the current project is not yet saved.
- **"Open project"**: Opens a project from a dialog.
- **"Open recent"**: Displays the list of the most recently used projects.
- **"Close project"**: Closes the current project. A warning may be displayed if the current project is yet not saved.
- **"Save project"**: Saves the current project.
- **"Save project as"**: Saves the current project from a dialog.
- **"Add project to group"**: Adds this project to the project group. See also the page dedicated to the [project groups](widgets_project_groups).
- **"Project editor"**: Displays widget used to edit the project properties,in respect with the format (CE or DUB).
- **"Edit project file"**: Opens the project file in a new source editor. When saved from a source editor, a project file is directly reloaded.
- **"View project command line"**: Displays the list of switches and arguments, as they would be passed to the compiler (or the build tool) when compiling.
- **"View in mini explorer"**: Expands the [mini-explorer](widgets_mini_explorer) tree on the folder that contains the project file.
- **"Verify project with Dscanner"**: Performs some static checks on each source of the project and using [Dscanner](https://github.com/dlang-community/D-Scanner).
- **"Compile project"**: Compiles the project using the current configuration.
- **"Compile and run project"**: Compiles the project using the current configuration and executes the output when the binary produced is executable.
- **"Compile and run project..."**: Ditto. Before the execution of the binary an input query dialog lets you pass options to the process.
- **"Run project"**: Executes the project output when the binary produced is executable.
- **"Run project..."**: Ditto. Before the execution, an input query dialog lets you specify switches and arguments to the process.
- **"Test project"**: Only for DUB projects. Invoke `dub test` using the configuration selected in the [project inspector](widgets_project_inspector).

#### Other build tools

It's possible to use the [custom tools](widgets_custom_tools) to call other build tools.
For example to call _make_, add a new tool with _make_ as _executable_ and sets the _workingDirectory_ to the folder that contains the makefile. 
To specify a special target, such as _release_, add _release_ in the parameters editor.
To get _make_ output in the messages, check _popUsesPipes_ in the tool options.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
