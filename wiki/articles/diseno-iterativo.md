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

Por el contrario, las metodologías adaptativas proponen el **diseño iterativo**, donde se asume que

-   sólo tenemos algunos procesos relevados, y aunque los tuviéramos en su totalidad, los requerimientos podrían cambiar.
-   es inocente pensar en que no habrá cambios en los requerimientos, dado que
    -   el usuario no sabe exactamente lo que se va a construir y tiene derecho a pedir modificaciones cuando se da cuenta de que cometió un error al dar información al diseñador.
    -   bajo la premisa anterior el diseñador no puede realizar un diseño que no esté sujeto a cambios, por los errores propios que además podría cometer.
-   si el diseño no es adecuado, debemos cambiarlo lo más pronto posible. Esto incluye la fase de codificación.
-   si queremos reflejar la realidad, tenemos que permitir que haya alternancia entre diseño y programación. No paralelizamos las actividades, sino que una se va solapando a la otra, como en una pila.
-   el diseño iterativo considera que los errores son parte del desarrollo mismo y necesitamos poder modificar el diseño en cualquier momento, sin que eso paralice el proyecto (iterativo tiene mucho de "prueba y error").

