---
layout: navpage
title: wiki.template landing page
---

# Articulos de la wiki
 
{% for page in site.pages %}
    {% if page.title and page.url contains 'wiki/articles/' %}
- [{{page.title}}]({{site.baseurl}}{{page.url}})
    {% endif %}
{% endfor %}

<br>