---
title: Widgets - DFMT commander
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

{% include xstyle.css %}

### DFMT commander

The _Dfmt commander_ widget is a simple but complete interface for the D source code formater [Dfmt](https://github.com/dlang-community/dfmt). 
The background tool has to be build and installed in one of the _PATH_ known by the operating system.

![](img/dfmt_commander.png)

A property inspector allows to tweak the format. 
See the [official documentation](https://github.com/dlang-community/dfmt#configuration) to learn more about the options.

- <img src="{%include icurl%}other/accept.png" class="tlbric"/>: apply the formatting. The editor state is backed up before each formatting.
- <img src="{%include icurl%}other/cancel.png" class="tlbric"/>: restore the previous backup.

The formatting is applied in memory and can be undone either from the widget or from the code editor. 
The changes are not physically applied until the file is explicitly saved.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
