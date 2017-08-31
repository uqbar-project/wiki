---
layout: article
title: Css
---

Es la forma más recomendada de especificar y modificar las cuestiones estéticas de una página [HTML](html.html). La característica principal es que **separa estas declaraciones del propio html**. Así evita ensuciar la información (html) con cuestiones estéticas (el archivo .css). Además permite reutilizar estilos entre las diferentes páginas html, manteniendo consistencia entre el estilo de todo el sitio/aplicación.

# Sintaxis

CSS tiene su propio lenguaje declarativo en el cual especificamos **reglas**. Una regla tiene la siguiente sintaxis:

![css rule](/img/css-rule.gif)

El **selector** nos permite aplicar el estilo a aquellos elementos que coincidan con dicha expresión. Luego el cuerpo de la regla contiene un conjunto de propiedades y valores para éstos.

Algunas propiedades aplican solo a algunos tipos de tags. Sin embargo, no es un lenguaje que "compile" o que tire errores. Simplemente si una propiedad no aplica a un tag, el browser no le va a dar bola.

Un primer ejemplo para entender la sintaxis

```css
span {
   text-align:center;
}
```

Aplica el valor "center" a la propiedad "text-align" de todos los tags de la página que sean de tipo &lt;span&gt; (en otras palabras, alinea un texto en el centro de donde está contenido).

# Selectores Principales

Básicamente lo primero que tenemos que saber sobre los selectores es que hay tres grandes tipos o formas de matchear nuestros tags.

-   **Por tag**, es decir a todos los tags de un tipo (por ejemplo h1, h2, etc).
-   **Por clase** (class), es decir a los tags a los que se les haya indicado un estilo determinado (mediante el atributo **class**).
-   **Por id**, es decir a un elemento específico de la página según su **id**.

Ejemplo por tag (ya vimos otro arriba para &lt;span&gt;)


```css
td {
   text-align:center;
   color:red;
}
```

Aplica esas dos propiedades a todos los &lt;td&gt;

```css
.filaImpar {
   text-align:center;
   color:red;
}
```

Este selector, que comienza con un punto, indica que va a matchear con cualquier tag (no importa el tipo de tag), siempre que éste tenga el valor **filaImpar** en su atributo **class**. Por ejemplo matchearía con estos tags:

```html
     <p class="filaImpar">Hola Soy un P&aacute;rrafo<p>

     <span class="filaImpar importante">Hola Soy un Span<p>

     <tr class="conBordes fondoImportante filaImpar">
        <td>Hola, soy una Fila</td>
     <tr>
```

Como se ve en el ejemplo anterior, un tag **puede tener más de un class**. Así los classes no tienen nada que ver con las clases de un lenguaje orientado a objetos. Pueden pensarlos más bien como "labels" o "etiquetas" o marcas que pongo a los tags, para luego por CSS agregarle características visuales. Así eventualmente uno en un proyecto grande, se crearía su propia convención con un conjunto de "classes" que reutilizaría en todo su sitio. Por ejemplo "titulo" o "menu", "botonGrande", "botonMediano", etc. Es una buena forma de elevar el nivel del html con nuevos significados.

El último ejemplo, matchear por id

```css
#unElementoEspecifico {
   text-align:center;
   color:red;
}
```

Este selector es el más "puntual" o específico, y permite matchear con tags, no importa su tipo, ni tampoco su class, sino que solo busca por "id".

```html
     <li id="opcionIrAAyuda">Ir a Ayuda</li>
     <button id="volver">Volver</button>
```

Como en una página no deberían existir dos tags con el mismo id sin importar en qué tag o nivel se encuentre es el mecanismo menos recomendable para trabajar.

# Uso de CSS desde HTML

El html usa el css mediante una declaración en la sección **HEAD**.

```html
      <html>
        <head>
          ...
          <link rel="stylesheet" type="text/css" href="styles.css" />
          ...
        </head>
      </html>
```

Donde "styles.css" sería mi archivo de estilos y estaría, en este caso en la misma carpeta en que se encuentra este html. También podríamos usar una URL absoluta:

```html
        <link rel="stylesheet" href="http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css">
```

Existen otras formas de incluir estilos en un html, sin embargo la mejor es la que ya citamos. Las demás involucran ensuciar el html, y las pueden ver [en este link](http://www.w3schools.com/css/css_howto.asp)

# Cascada

Las reglas se aplican *en cascada*, esto significa dos cosas:

1.  En primer lugar cada componente hereda determinados estilos de sus contenedores, por ejemplo un td (celda de una tabla) hereda los del tr (fila) y del table correspondientes. Los estilos que apliquen al componente específico sobreescriben a los del contenedor, pero aquellos que no estén especificados se heredan. No todas las indicaciones de estilo son "heredables" (inheritable en inglés), es importante entender el comportamiento de cada una de las diferentes indicaciones de estilo.
2.  En segundo lugar sobre cada componente pueden aplicarse más de un estilo, que matcheen con ese componente según su tag, class y id respectivamente. Esos diferentes estilos se van a combinar permitiendo que el estilo más específico sobreescriba los estilos más generales, pero aun manteniendo las indicaciones correspondientes al estilo más general que no sean redefinidas.

Ejemplo de varias reglas aplicando al mismo tiempo sobre un tag.

```css
tr {
   text-align:center;
}

.resaltar {
   background-color: red;
}
```


Ambas reglas van a aplicar en este tag

```html
        <tr class="resaltar">
            <td>Hola, soy una celda</td>
        </tr>
```

El texto se va a ver centrado y además con fondo rojo.

Combinando Selectores (OR)
--------------------------

Es común que tengamos que aplicar los mismos estilos a diferentes tags. Para evitar duplicación de código las reglas se pueden combinar. Ej:

```css
h1 {
  text-align:center;
  color:red;
}
h2 {
  text-align:center;
  color:red;
}
p {
  text-align:center;
  color:red;
}
```

Se puede refactorizar a esto:

```css
h1, h2, p {
  text-align:center;
  color:red;
}
```

La sintaxis entonces es:

```css
     selector1, selector2, ... selectorN {
         propiedad1: valor1;
         propiedad2: valor2;
         ...
         propiedadN: valorN;
     }
```

Se puede pensar como un **or**. Sería, si es un h1 **o** es un h2, **o** es un p.

Ventajas del uso de CSS
-----------------------

La separación de concerns (layout vs. configuración estética) permite dos cosas:

-   Configurar y personalizar el estilo que se muestre por usuario/región/dispositivo y preservar el lema DRY (Don't repeat yourself), la página HTML aumenta su expresividad (es más fácil de entender) los diseñadores gráficos se concentran el look & feel, decorando los botones, las tablas (grillas), el tipo de letra, los títulos y también los logos, isotipos e imágenes que la aplicación va a tener. Por lo general reciben un "esqueleto" de las páginas con datos de prueba escritos en forma manual

-   Por otra parte, los programadores se concentran en la lógica de presentación y en la facilidad de uso por parte del usuario

# Links relacionados

-   [Temario Algoritmos III](algo3-temario.html)

