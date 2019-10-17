---
title: Widgets - Profile viewer
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

{% include xstyle.css %}

### Profile viewer

#### Description

The _profile viewer_ widget displays the results stored in the _trace.log_ file that a software compiled with DMD outputs when it's compiled with the `-profile` switch.

![](img/profile_viewer.png)

The pie displays the weight of a each function for a particular criterion.
This criterion can be selected in the combo box that's located in the toolbar.

The list displays all the results, which can be inspected more accurately after sorting a column.

#### Toolbar

- <img src="{%include icurl%}other/list.png" class="tlbric"/>: Loads the _trace.log_ file located in the project output path.
- <img src="{%include icurl%}folder/folder.png" class="tlbric"/>: Proposes to open the _trace.log_ from a dialog.
- <img src="{%include icurl%}arrow/arrow_update.png" class="tlbric"/>: Reloads the current _trace.log_ or tries to load it from the current directory.
- <img src="{%include icurl%}cog/wrench.png" class="tlbric"/>: Shows the profile viewer options.

#### Options

- **hideAtributes**: Sets if the functions attributes are displayed.
- **hideRuntimeCalls**: When checked, all the functions starting with `core.` are excluded.
- **hideStandardLibraryCalls**: When checked, all the functions starting with `std.` are excluded.
- **otherExclusion**: Allows to define other sub-strings masks.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
