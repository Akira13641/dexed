---
title: Widgets - project inspector
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

{% include xstyle.css %}

### Project inspector

The project inspector is used to

- select the project configuration.
- open sources in an new editor.
- add or remove source if the active project has the [CE format](features_projects).

![](img/project_inspector.png)

The following toolbar buttons are always available:

- <img src="{%include icurl%}arrow/arrow_update.png" class="tlbric"/>: Updates the list of sources files and auto fetch DUB dependencies when applicable.
- <img src="{%include icurl%}folder/folders_explorer.png" class="tlbric"/>: Sets if the sources are displayed in a tree rather than in a single node.

The following toolbar buttons are only visible for CE projects:

- <img src="{%include icurl%}file/document_add.png" class="tlbric"/>: Adds a D source to the project from a dialog. The new source is not directly opened in the editor. To add a file that is already edited, rather use **"Add file to project"** from the **File** menu.
- <img src="{%include icurl%}file/document_delete.png" class="tlbric"/>: Removes from the project the source that's selected in the tree.
- <img src="{%include icurl%}folder/folder_add.png" class="tlbric"/> Adds a folder of D source to the project from a dialog. The procedure is recursive.
- <img src="{%include icurl%}folder/folder_delete.png" class="tlbric"/> Removes from the project the sources files that stand in the same directory as the source selected in the tree.

Note that instead of using the dialogs to add files, it's also possible to drop items from a file explorer.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
