---
layout: article
title: Html
featured: true
---

# HTML

El HTML es el lenguaje principal que se utiliza para construir páginas web. Es un lenguaje declarativo que permite describir la información a mostrar en una página web. Esta información es interpretada por un web browser que normalmente obtiene las diferentes páginas HTML de un web server y las muestra al usuario.

# Estructura General de una página HTML

El HTML es un lenguaje de etiquetas o marcas. Por cada etiqueta que abre hay otra igual pero que cierra, se escribe igual pero con una "/" adelante.

Una página HTML se divide en dos secciones: head y body. El head describe información que no conforma la página en sí (no es visible) pero la describe. El body contiene la parte que se puede ver.

Un ejemplo básico:

```html
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>
    Hola Mundo!
    </title>
</head>
<body>
    Hola Mundo`
</body>
</html>
```

<!-- -->

# Elementos básicos de una página HTML

## Links

Se indican con el tag **a** y permiten vincular diferentes páginas HTML entre sí, haciéndolas navegables. Los atributos básicos de un link son:

-   **href** es una URL que puede ser relativa o absoluta e indica a qué página debe dirigirse el web browser cuando el link es clickeado.

La parte visible del link será dada por el cuerpo del link, por ejemplo:

```html
<a href="otraPag.html">Este es el texto que se vería</a>
```

## Tablas
