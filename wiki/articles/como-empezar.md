---
layout: article
title: Como contribuir a la wiki
---

# Introducción

Esta sección describe brevemente la sintaxis de markdown que se usa en la wiki, además de mostrar 
algunas cosas extras que componen a la wiki.

Para hacer modificaciones simples con un articulo, basta con modificar desde github directamente el ariculo en cuestion.
En caso de que se desee modificar 


La carpeta wiki/articles contiene todos los articulos de la wiki, incluyendo esta sección.

## Requerimientos

Para testear localmente se necesitan los siguientes requerimientos previos.

* Ruby 2.2.0
* Bundler
* bower
* imagemagick
* python
* pygments


Para instalar las dependencias de ruby adicionales ejecutar:

```
bundle install
```

Este proyecto utiliza [Jekyll](http://jekyllrb.com/), para levantar una instancia local una vez instaladas las dependencias habrá que instalar
las dependencias adicionales mediante bower:

```
bower install
```

Una vez ya resueltas todas las dependencias se puede ya levantar el entorno mediante:

```
jekyll s
```

Esto armar un build local y una vez completado este proceso levanta localmente la instancia. 
De aqui en más se podrá hacer modificaciones localmente, pudiendo ver las modificaciones previo a enviar estos cambios
al repositorio de github. 

Tener en cuenta que ante cualquier modificacion fuera de los articulos, como los plugins, se necesitará ejecutar un 
nuevo build de jekyll. (El servidor se levantará por default en `localhost:4000`)

## Modificando

Una vez levantado localmente nuestra instancia de jekyll con la wiki, podremos modificar los articulos que componen
la wiki en sí. Todos los articulos están situados en  `./wiki/articles`, se pueden tener subdirectorios tambien, como
por ejemplo `.wiki/articles/sub/Subpage.html`

Todos los articulos tienen que tener el header yml:

```markdown
---
layout: article
title: <title>
---
```

Despues de eso el formato de los articulos estan en markdown, todos los articulos deben tener el layout `article` 
El campo `title` es utilizado para Describir el titulo principal en el artículo.

# Syntaxis adicional

## Linkeando artículos

Se puede linkear un artículo de dos maneras

1. Se puede utilizar el plugin `link_article`:
    ```ruby
        {% link_article como empezar %}
    ```
2. O Hacerlo en markdown directamente:
   `[The guide for newbies](/wiki/articles/como-empezar.html)`

Ambos enfoques generan el mismo html, solamente que en el primero se omite la extensión y los guiones medios (-).

## Embebiendo gists

Se puede embeber gists publicos de la siguiente manera:

```ruby
gist bossiernesto/97285bb96f1da185858af7de5cdee978
```

Esto genera:

{% gist bossiernesto/97285bb96f1da185858af7de5cdee978 %}

## Bibliografía

Se puede citar referencias mediante la sintaxis

```ruby
    cite "nombre de cita"
```
esto genera:

{% cite Burmako_scalamacros %}

Ahora existe un repositorio central de bibliografia en `_bibliography/wikibiblio.bib`. Todas las referencias deben estar
anotadas con una sintaxis BibTex, la misma se puede obtener en algun sitio que nos brinde este tipo de formato como [CiteSeerx](http://citeseerx.ist.psu.edu/)

Si se quiere mostrar toda la bibliografía del repositorio se tiene que usar la anotación `bibliography`

{% bibliography %}

Ahora si queremos solo mostrar las bibliografía citada, utilizaremos la anotación `bibliography --cited`

{% bibliography --cited %} 

Para mayores referencias ver la siguiente sección

## Embeber markdown remoto

Se puede embeber markdown en formato raw de un repositorio externo de la organizacion mediante la notación

` remote_markdown <url remota>`

{% wiki_note_alert %}
  Cuando se utilice esta funcionalidad, verificar previamente que renderiza el link y el origen del mismo. Se recomienda
  que sea de un sitio como Github o Gitlab y que sea mediante una url por https.
{% endwiki_note_alert %}

## Embebiendo código

Se puede incluir código con el triple backtrick. Esta wiki usa `highlightjs` para syntax highlighting. 
[highlightjs](https://highlightjs.org/) tiene muchos esquemas y 
[aquí hay algunos ejemplos](https://highlightjs.org/static/demo/).

{% wiki_note_info %}
Hay que tener en cuenta que se debe configurar el idioma cuando se utiliza la sintaxis de triple \`. 
El resaltado no funciona si no hay un idioma especificado.
{% endwiki_note_info %}

Ejemplo:
```ruby
    class Bleh
        def a(b)
            b + 1
        end
    end
```


## Alertas

Hay distinto tipos de alertas:

{% wiki_note_secondary %}
  secondary
{% endwiki_note_secondary %}

{% wiki_note_info %}
  info
{% endwiki_note_info %}

{% wiki_note_success %}
  success
{% endwiki_note_success %}

{% wiki_note_warning %}
  warning
{% endwiki_note_warning %}

{% wiki_note_alert %}
  alert
{% endwiki_note_alert %}

Cada tipo de alerta tiene un color distinto, la sintaxis es la siguiente:

```ruby
  "{% wiki_note_secondary %}"
    your text
  "{% endwiki_note_secondary %}"
```

Donde `<kind>` puede ser "secondary", "info", "success ", "warning" or
"alert".

Se agrega ahora el readme de jekyll-scholar con las referencias

{% remote_markdown https://raw.githubusercontent.com/inukshuk/jekyll-scholar/master/README.md %}