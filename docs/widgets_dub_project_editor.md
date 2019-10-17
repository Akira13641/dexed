---
title: Widgets - DUB project editor
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

{% include xstyle.css %}

### DUB project editor

The DUB project editor allows to edit, add and remove properties to a a DUB project that has the [JSON format](http://code.dlang.org/package-format?lang=json).
DUB projects with the [SDL format](http://code.dlang.org/package-format?lang=sdl) are opened in read only mode.

![](img/dub_project_editor.png)

A property value can be modified in the field at the bottom. New values always require an extra validation.
New properties can be added or removed:

- <img src="{%include icurl%}other/textfield_add.png" class="tlbric"/>: Shows a small dialog that allows to add a new value, a new array or a new object.
- <img src="{%include icurl%}other/textfield_delete.png" class="tlbric"/>: Removes the selected property. Note that the effect is not reflected until the project is saved as a file (since Dexed does not communicate directly with DUB).
- <img src="{%include icurl%}other/copy.png" class="tlbric"/>: Duplicates the selected object. Can be used to clone a configuration or a build type.
- <img src="{%include icurl%}arrow/arrow_update.png" class="tlbric"/>: Updates the list of sources files and auto fetch dependencies if specified as an option for [DUB](options_dub_build)

![](img/dub_add_property.png)

There's two ways to add a property:

* Type the property name and select its JSON type.
* Select a property name in the combo box.

When the second method is used the property type is selected automatically, which is safer.
The property name is not always required. For example when when an array item is added the content of the field is ignored.
After adding a property, its value still needs to be set at the bottom of the tree.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
