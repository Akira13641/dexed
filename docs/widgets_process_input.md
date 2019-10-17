---
title: Widgets - process input
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

{% include xstyle.css %}

### Process input

The process input widget is used to pass input to the processes that are executed within Coedit.

![](img/process_input.png)

- <img src="{%include icurl%}other/pencil_go.png" class="tlbric"/>: Sends the text to the process, also works with <kbd>ENTER</kbd>.
- <img src="{%include icurl%}other/pencil_delete.png" class="tlbric"/>: Closes the process input stream.
- <img src="{%include icurl%}other/cancel.png" class="tlbric"/>: Forces the process termination.

The input text can contain [symbolic strings](features_symbolic_strings).

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
