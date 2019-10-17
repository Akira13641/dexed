---
title: Widgets - library manager
--- 

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

{% include xstyle.css %}

### Library manager

#### Description

The library manager (aka the _libman_) is designed to replace the _sc.ini_ or the _sc.conf_ file to manage the static libraries.
Despite of its simplistic look it's a central component of the software. The [completion](features_dcd), the [runnables](features_runnables), the CE project format strongly rely on it.

The _libman_ links the library files (_*.lib_ or _*.a_) to their D sources and are represented with a simple **alias** that is used in different part of the software.
For example, in a CE project, you don't need to specify the **-I** path nor its matching library file name, this is done automatically.

![](img/library_manager.png)

#### Import analysis

Since version 3, the _libman_ also contains a database that links each item to a list of module.
This is used to automatically detect a dependency from a simple import declaration.
This features is only used for the CE project format and the [runnables](features_runnables) modules.

#### Toolbar

- <img src="{%include icurl%}book/book_add.png" class="tlbric"/>: Adds a library item, by default its fields are initialized with _n/a_.
- <img src="{%include icurl%}book/book_delete.png" class="tlbric"/>: Removes the selected item.
- <img src="{%include icurl%}arrow/arrow_up.png" class="tlbric"/> **/** <img src="{%include icurl%}arrow/arrow_down.png" class="tlbric"/> Changes selected entry position.
- <img src="{%include icurl%}book/book_link.png" class="tlbric"/>: If the current project is a static library then the _libman_ will use its parameters to create an entry. This avoids to browse in the dialogs. Note that sometimes the sources root folder has to be adjusted.
- <img src="{%include icurl%}book/book_open.png" class="tlbric"/>: If the selected item defines a _project_ then closes current project and opens the one matching to the entry.
- <img src="{%include icurl%}other/dub.png" class="tlbric"/>: Allows to fetch the master version of a [DUB registry item](http://code.dlang.org/). See the last section.
- <img src="{%include icurl%}book/book.png" class="tlbric"/>: enable or disable the selected entry. This is mainly useful to test new DMD versions since the libraries often have to be recompiled from a version to another.
- <img src="{%include icurl%}book/book_edit.png" class="tlbric"/>: edit the item alias.
- <img src="{%include icurl%}folder/folder_brick.png" class="tlbric"/>: select the library file. In some rare case, this field can be omitted (for example if the library file is set in the _sc.ini_).
This can be skipped if the library is only a small orphan _d_ source that's not been compiled as a static library or if the entry is only used for the [DCD](features_dcd) completion.
- <img src="{%include icurl%}other/bricks.png" class="tlbric"/>: select a folder that contains several libraries files or several orphans modules.
- <img src="{%include icurl%}folder/folder_add.png" class="tlbric"/>: select the sources root folder. (the one commonly named 'import').
- <img src="{%include icurl%}other/script_bricks.png" class="tlbric"/>: set the project file that matches to the selected item.

#### Register online DUB packages

When the toolbar button that displays DUB logo is clicked, a dialog is displayed. It used to select an online package that will be downloaded, compiled and automatically installed in the _libman_

![](img/dub_register_package.png)

The text field is used to select the name of the package to install.

- <img src="{%include icurl%}arrow/arrow_update.png" class="tlbric"/>: Update the list of the online packages.
- <img src="{%include icurl%}other/tag_purple.png" class="tlbric"/>: By the default the master version of the package is installed. When the tag button is down Dexed rather installs the package from its latest tag.
- <img src="{%include icurl%}other/accept.png" class="tlbric"/>: Closes the dialog and start downloading before installing.
- <img src="{%include icurl%}other/cancel.png" class="tlbric"/>: Cancels and does nothing.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
