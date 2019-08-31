---
layout: article
title: Html
categories: [web, html, introduccion]
featured: true
---

# HTML

HTML (_HyperText Markup Language_) es el lenguaje principal que se utiliza para construir páginas web. Es un lenguaje declarativo que permite describir el contenido de una página.

# Estructura General de una página HTML

## Sintaxis

El HTML es un lenguaje de etiquetas o marcas. Por cada etiqueta que abre hay otra igual pero que cierra, se escribe igual pero con una "/" adelante.

```html
<html>
</html>

<h3>
</h3>
```

La notación inline permite abrir y cerrar el tag en un mismo paso:

```html
<img />
<br/>
<hr/>
```

Si bien muchos browsers relajan la sintaxis de apertura y cierre de tags (como en los casos de `<ul>` y `<br>`) es una buena práctica respetar esta idea, lo que permite definir [XHTML](https://en.wikipedia.org/wiki/XHTML) o HTMLs "bien formados".

## Tag HTML

Cada tag puede tener

- contenido, que es un texto
- atributos, demarcados con una clave y un valor
- otros tags, que se anidan en forma jerárquica

```html
<tag atributo1="valor1" atributo2="valor2">
    contenido con <otrotag>otro subcontenido</otrotag>
</tag>
```

![htmlStructure](/img/wiki/htmlStructure.png)

## Estructura de una página HTML

El contenido de una página web se estructura en forma jerárquica. A partir de [HTML 5](https://developer.mozilla.org/es/docs/Sections_and_Outlines_of_an_HTML5_document) tenemos tags especiales que permiten definir una semántica estándar para nuestro documento:

- un encabezado o _header_ principal de la página
- el cuerpo principal o _body_, que arma el contenido de la página y puede agrupar tags visuales y los de contenido, como 
  - _section_ y _article_: que sirven como elementos que agrupan contenido, y que pueden a su vez tener elementos head, footer o nuevos section/article
  - _nav_: que contiene links de navegación
  - _aside_: elementos que no forman parte del contenido principal 
- un pie de página o _footer_ principal de la página

![htmlContent](/img/wiki/htmlContent.png)

### Ejemplo

Un ejemplo posible de estructuración del body de una página puede ser:

```html
<section>
  <h1>El pato</h1>
  <section>
    <h1>Introducción</h1>
    <p>En esta sección, ampliaremos nuestro concepto del pato.</p>
  </section>
  <section>
    <h1>Hábitat</h1>
    <p>Esta especie de animal llegó en tiempos remotos, traídos en diversas expediciones desde Europa y China, se fueron expandiendo por todo el mundo rápidamente y debido a su temperamento fueron criados como animales domésticos para el consumo de su carne y huevos, así como para mascotas en muchos hogares.</p>
  </section>
   <aside>
    <p>otros estudiosos del pato</p>
  </aside>
</section>
<footer>
  <p>(c) 2010 The Example company</p>
</footer>
```

### Análisis semántico de un HTML

Esto permite que haya plugins como el [HTML 5 Outliner](https://chrome.google.com/webstore/detail/html5-outliner/afoibpobokebhgfnknfndkgemglggomo) que pueda ordenar los elementos según su orden de importancia (la jerarquía la define el nivel de anidamiento dentro de HTML pero también el tag le dice semánticamente a qué corresponde cada contenido).

![html5Outliner](/img/wiki/html5Outliner.gif)

Aquí vemos que algunas páginas están semánticamente mejor ordenadas que otras: a veces no hay un título que identifique la sección, o no se respeta bien el orden jerárquico de los contenidos.

En Firefox tenemos el add-on [headingsMap](https://addons.mozilla.org/es/firefox/addon/headingsmap/) que construye una estructura en paralelo para navegar las páginas de otra manera (para por ejemplo ordenar las noticias según las más leídas):

![headingsMap](/img/wiki/headingsMap.gif)

# Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
- [W3 - Web Architecture](https://www.w3.org/standards/webarch/)
