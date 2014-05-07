Es la forma más recomendada de especificar y modificar las cuestiones estéticas de una página [HTML](html.html). La característica principal es que **separa estas declaraciones del propio html**. Así evita ensuciar la información (html) con cuestiones estéticas (el archivo .css). Además permite reutilizar estilos entre las diferentes páginas html, manteniendo consistencia entre el estilo de todo el sitio/aplicación.

Sintaxis
--------

CSS tiene su propio lenguaje, declarativos en el cual especificamos **reglas**. Una regla tiene la siguiente sintaxis:

![](css-rule.gif "css-rule.gif")

El **selector** es una expresión que nos permite matchear aquellos elementos a los que queremos aplicarle estos estilos. Luego el cuerpo de la regla contiene un conjunto de propiedades y valores para estos.

Algunas propiedades aplican solo a algunos tipos de tags. Sin embargo, no es un lenguaje que "compile" o que tire errores. Simplemente si una propiedad no aplica a un tag, el browser no le va a dar bola.

Un primer ejemplo para entender la sintaxis

`span {`
`   text-align:center;`
`}`

Aplica el valor "center" a la propiedad "text-align" de todos los tags de la página que sean de tipo <span>.

Selectores
----------

Básicamente lo primero que tenemos que saber sobre los selectores es que hay tres grandes tipos o formas de matchear nuestros tags.

-   **Por tag**, es decir a todos los tags de un tipo (por ejemplo h1, h2, etc).
-   **Por clase** (class), es decir a los tags a los que se les haya indicado un estilo determinado (mediante el atributo **class**).
-   **Por id**, es decir a un elemento específico de la página según su **id**.

Ejemplo por tag (ya vimos otro arriba para <span>)

`td {`
`   text-align:center;`
`   color:red;`
`}`

Aplica esas dos propiedades a todos los

<td>
`.filaImpar {`
`   text-align:center;`
`   color:red;`
`}`

Este selector, que comienza con un punto, indica que va a matchear con cualquier tag (no importa el tipo de tag), siempre que éste tenga el valor **filaImpar** en su atributo **class**. Por ejemplo matchearía con estos tags:

&lt;p class="filaImpar"&gt;Hola Soy un Párrafo&lt;p&gt; &lt;span class="filaImpar importante"&gt;Hola Soy un Span&lt;p&gt; &lt;tr class="conBordes fondoImportante filaImpar&gt; &lt;td&gt;Hola, soy una Fila&lt;/td&gt; &lt;tr&gt;

Como se ve acá, un tag **puede tener más de un class**. Así los classes no tienen nada que ver con las clases de un lenguaje orientado a objetos. Pueden pensarlos más bien como "labels" o "etiquetas" o marcas que que hago a los tags, para luego por CSS agregarle características visuales. Así eventualmente uno en un proyecto grande, se crearía su propia convención con un conjunto de "classes" que reutilizaría en todo su sitio. Por ejemplo "titulo" o "menu", "botonGrande", "botonMediano", etc. Es una buena forma de elevar el nivel del html con nuevos significados.

El último ejemplo, matchear por id

`#unElementoEspecifico {`
`   text-align:center;`
`   color:red;`
`}`

Este selector es el más "puntual" o específico, y permite matchear con tags, no importa su tipo, ni tampoco su class, sino que solo busca por "id".

<li id="opcionIrAAyuda">
Ir a Ayuda

</li>
<button id="volver">`Volver`</button>

Algo importante para entender esto, es que en una página no deberían existir dos tags con el mismo id. No importa si están dentro de diferentes tags o en diferentes niveles. El "id" es único para todos los elementos de la página.

Cascada
-------

Las reglas se aplican *en cascada*, esto significa dos cosas:

1.  En primer lugar cada componente hereda determinados estilos de sus contenedores, por ejemplo un td hereda los del tr y del table correspondientes. Los estilos que apliquen al componente específico sobreescriben a los del contenedor, pero aquellos que no estén especificados se heredan. No todas las indicaciones de estilo son "heredables" (inheritable en inglés), es importante entender el comportamiento de cada una de las diferentes indicaciones de estilo.
2.  En segundo lugar sobre cada componente pueden aplicarse más de un estilo, que matcheen con ese componente según su tag, class y id respectivamente. Esos diferentes estilos se van a combinar permitiendo que el estilo más específico sobreescriba los estilos más generales, pero aún manteniendo las indicaciones correspondientes al estilo más general que no sean redefinidas.

