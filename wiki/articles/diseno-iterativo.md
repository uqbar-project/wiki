La metodología de desarrollo nos lleva a diseñar de dos formas completamente diferentes:

Las metodologías predictivas proponen el **diseño anticipado**, donde se asume que

-   el análisis ya ha relevado todos los procesos que el usuario necesita
-   tenemos disponible toda la información para poder definir cada proceso
-   no habrá cambios en los requerimientos hasta nuestra implementación
    -   el usuario no hará cambios o nuevos pedidos.
    -   no habrá cambios externos al sistema que obliguen a modificarlo (como disposiciones legales).
-   si el diseño es adecuado, la codificación se ajustará perfectamente a lo que el usuario necesita.
    -   para ello hay que documentar el sistema en su completitud para que los programadores no tengan que tomar decisiones de diseño en la codificación.
-   en la fase de diseño no se debe programar, dado que se estaría solapando la actividad (de la misma manera que en la fase de codificación no se debe diseñar)

Esta forma de diseñar también se conoce como "Big Design Up Front". [En este artículo](http://www.joelonsoftware.com/articles/AardvarkSpec.html) el autor expone algunos argumentos en favor de esta metodología.

Por el contrario, las metodologías adaptativas proponen el **diseño iterativo**, donde se asume que

-   sólo tenemos algunos procesos relevados, y aunque los tuviéramos en su totalidad, los requerimientos podrían cambiar.
-   es inocente pensar en que no habrá cambios en los requerimientos, dado que
    -   el usuario no sabe exactamente lo que se va a construir y tiene derecho a pedir modificaciones cuando se da cuenta de que cometió un error al dar información al diseñador.
    -   bajo la premisa anterior el diseñador no puede realizar un diseño que no esté sujeto a cambios, por los errores propios que además podría cometer.
-   si el diseño no es adecuado, debemos cambiarlo lo más pronto posible. Esto incluye la fase de codificación.
-   si queremos reflejar la realidad, tenemos que permitir que haya alternancia entre diseño y programación. No paralelizamos las actividades, sino que una se va solapando a la otra, como en una pila.
-   el diseño iterativo considera que los errores son parte del desarrollo mismo y necesitamos poder modificar el diseño en cualquier momento, sin que eso paralice el proyecto (iterativo tiene mucho de "prueba y error").
-   simplest thing that could possible work
-   [| En este artículo](http://martinfowler.com/articles/designDead.html) Martin Fowler explica el diseño desde el punto de vista de las metodologías ágiles.

El costo del cambio
-------------------

Tradicionalmente las metodologías secuenciales interpretan que le costo de corregir un error o de introducir un cambio en un desarrollo de software se incrementa exponencialmente a medida que se avanza con el desarrollo. Según esta visión, un error en el análisis que podría corregirse en un par de horas de trabajo, tardará días en solucionarse si se lo detecta durante la etapa de diseño e incluso podría requerir de varias semanas si se lo encuentra recién durante la etapa de construcción.

Varios autores, entre ellos Kent Beck han propuesto otra interpretación. Según esta nueva perspectiva la curva del cambio es radicalmente distinta para los proyectos de software en la actualidad. La valoración incorrecta del costo del cambio proviene en parte por la asociación con otras ingenierías; en las ingenierías que trabajan con entidades físicas hacer una modificación sobre algo construido suele ser muy costoso, incluso en algunos casos imposible. Esto no es válido para una ingeniería que trabaja con productos tan abstractos y maleables como el software. Adicionalmente la ingeniería del software se diferencia radicalmente de las otras en cuanto a que el costo de reproducción de un producto una vez construido es insignificante (compárese el costo de construir una nueva unidad de un automóvil determinado en una línea de producción con el costo de producir una nueva unidad de un programa que se vende en forma masiva).

Por otro lado, las herramientas para el desarrollo de software han evolucionado notablemente en los últimos años, apareciendo entornos integrados de desarrollo, lenguajes de muy alto nivel, herramientas de versionado, refactorings automáticos, herramientas de integración contínua, entre otros. A modo de ejemplo, podemos considerar el costo de cambiar el nombre de una variable en un entorno de desarrollo que no tenga la capacidad de hacerlo automáticamente en un programa grande con la simplicidad de hacerlo en un entorno de los que se usan hoy en día. El tiempo se reduce a penas a segundos.

Según la interpretación de Beck, cualquier cambio es realizable con un costo relativamente bajo aún en etapas avanzadas del desarrollo. Otros autores sugieren que hay algunos cambios para los cuales se puede aplanar la curva del costo y otros cambios para los que no. Por ejemplo, se ha sugerido que los errores arquitecturales tienden a tener curvas empinadas. Si bien es imaginable que en la medida que las herramientas (tanto tecnológicas como conceptuales) evolucionen, más y más decisiones puedan tomarse considerando la posibilidad de modidicarlas mas adelante sin que esto implique un costo alto, todavía hoy en día es importante detectar para cada decisión que tomamos en una etapa temprana del desarrollo si el costo potencial de un error es elevado o no.
