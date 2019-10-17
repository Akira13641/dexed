---
title: Widgets - mini explorer
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}
{% include xstyle.css %}

### Mini explorer

#### Description

The mini explorer provides basic file browsing functionality within the IDE.

![](img/mini_explorer.png)

- <img src="{%include icurl%}folder/folder_go.png" class="tlbric"/>: When clicked, allows to select a custom tree root. When using the associated drop down, allows to select a particular drive as root.
- <img src="{%include icurl%}arrow/go_previous.png" class="tlbric"/>: Got to the root parent folder.
- <img src="{%include icurl%}folder/folder_add.png" class="tlbric"/>: Adds the selected folder to the favorites.
- <img src="{%include icurl%}folder/folder_delete.png" class="tlbric"/>: Removes the selected favorite folder.
- <img src="{%include icurl%}other/flash.png" class="tlbric"/>: Open the selected folder or execute the selected file using the shell.
- <img src="{%include icurl%}other/pencil.png" class="tlbric"/>: If the selected file is a CE or a DUB project then opens it as a project otherwise opens it in a new code editor.
- ***input field***: filter the files whose name contains the text typed.

The file list supports drag and drop.

#### Options

A few options are available in the [option editor](widgets_options_editor).

![](img/options_mini_explorer.png)

- **contextExpands**: If checked then the tree auto expands to the folder that contains the source or the project file that's been focused.
- **doubleClick**: Defines what happens when a file is double clicked.
- **showHidden**: Sets if hidden folders and files are displayed.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
