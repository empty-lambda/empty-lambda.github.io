---
layout: page
title: blogging functionally 

---
{% include JB/setup %}

Recording fragments and bits from learning.

紀錄點滴碼農人生


-----

## [show p | p <- Posts]

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>



