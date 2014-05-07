Es la forma más recomendada de especificar y modificar las cuestiones estéticas de una página [HTML](html.html). La característica principal es que **separa estas declaraciones del propio html**. Así evita ensuciar la información (html) con cuestiones estéticas (el archivo .css). Además permite reutilizar estilos entre las diferentes páginas html, manteniendo consistencia entre el estilo de todo el sitio/aplicación.

Sintaxis
--------

CSS tiene su propio lenguaje, declarativos en el cual especificamos **reglas**. Una regla tiene la siguiente sintaxis:

![](css-rule.gif "css-rule.gif")

El **selector** es una expresión que nos permite matchear aquellos elementos a los que queremos aplicarle estos estilos. Luego el cuerpo de la regla contiene un conjunto de propiedades y valores para estos.

Algunas propiedades aplican solo a algunos tipos de tags. Sin embargo, no es un lenguaje que "compile" o que tire errores. Simplemente si una propiedad no aplica a un tag, el browser no le va a dar bola.

Selectores
----------

Básicamente lo primero que tenemos que saber sobre los selectores es que hay tres grandes tipos o formas de matchear nuestros tags.

-   **Por tag**, es decir a todos los tags de un tipo (por ejemplo h1, h2, etc).
-   **Por clase** (class), es decir a los tags a los que se les haya indicado un estilo determinado (mediante el atributo **class**).
-   **Por id**, es decir a un elemento específico de la página según su **id**.

Por ejemplo:

`td, th {`
`}`
`.filaImpar {`
`}`
`#unElementoEspecifico {`
`}`

Entre las llaves se colocarían las indicaciones de estilo.

Las reglas se aplican *en cascada*, esto significa dos cosas:

1.  En primer lugar cada componente hereda determinados estilos de sus contenedores, por ejemplo un td hereda los del tr y del table correspondientes. Los estilos que apliquen al componente específico sobreescriben a los del contenedor, pero aquellos que no estén especificados se heredan. No todas las indicaciones de estilo son "heredables" (inheritable en inglés), es importante entender el comportamiento de cada una de las diferentes indicaciones de estilo.
2.  En segundo lugar sobre cada componente pueden aplicarse más de un estilo, que matcheen con ese componente según su tag, class y id respectivamente. Esos diferentes estilos se van a combinar permitiendo que el estilo más específico sobreescriba los estilos más generales, pero aún manteniendo las indicaciones correspondientes al estilo más general que no sean redefinidas.

