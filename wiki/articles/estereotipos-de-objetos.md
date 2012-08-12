Un objeto presenta presentan tres características fundamentales:

-   Comportamiento: un objeto sabe hacer cosas, y eventualmente cada uno las podría saber hacer de distinta forma.
-   Estado: un objeto mantiene relaciones con otros objetos, las cuales pueden cambiar a lo largo de su vida. Muchas veces es útil pensar
-   Identidad: un objeto es tratado mediante referencias, y puede ser distinguido de otro, aun cuando el otro presente el mismo estado y comportamiento.

Los objetos existen en nuestro sistema para cumplir responsabilidades. Aunque normalmente asociamos fuertemente la idea de responsabilidad a la de comportamiento hay ocasiones en que nos convendrá diseñar nuestros objetos de forma tal que expongan su estado e identidad.

En base a estos tres aspectos del objeto, podemos ensayar la siguiente caracterización de los objetos:

-   Servicios
-   Entidades
-   Símbolos
-   Valores

Algunas combinaciones nos llevan a: Identidad importante, estado y comportamiento inexistentes: símbolos, candados. Identidad y estado importante: entidades Comportamiento importante, estado e identidad insignificantes: objeto función, stratey stateless,

`Comportamiento ...`

Semántica de referencia vs semántica de valor

-   Su identidad no es importante
-   (Consecuencia de lo anterior) Su estado, si importante, es constante.

