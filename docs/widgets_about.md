---
title: About box
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

{% include xstyle.css %}

The _about box_ displays the software version and the information about the tools used in background.

![](img/about.png)

The status of each tool is indicated:

- <img src="{%include icurl%}bullet/bullet_green.png" class="tlbric"/>: The tool is well detected.
- <img src="{%include icurl%}bullet/bullet_yellow.png" class="tlbric"/>: The tool cannot be found but it's not necessary. Expect some features not to work properly even if the software is still usable.
- <img src="{%include icurl%}bullet/bullet_red.png" class="tlbric"/>: The tool cannot be found but it's necessary. The software is not usable. This happens when the tool cannot be located using the PATH variable.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}
