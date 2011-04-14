La posibilidad de definir funciones utilizando aplicación parcial en la propia definición trae aparejado a veces la dificultad para entender cuántos parámetros espera esa función que se está definiendo.

Por ejemplo la función es múltiplo así definida tiene dos parámetros:

`` esMultiploDe a = (==0).( `mod` a) ``

La regla básica
---------------

La regla es que una función como esa tiene tantos parámetros como la suma entre:

-   Los parámetros que aparecen a la izquierda del igual.
-   Los parámetros que "le faltan" a la expresión a la derecha del igual.

Analizando el caso anterior vemos:

-   Un parámetro a la izquierda del igual: a. (Esta parte es la más directa.)
-   La expresión de la derecha es una composición y por lo tanto una función a-&gt;b entonces le falta un parámetro.
-   En total la función recibe dos parámetros.

Un detalle adicional
--------------------

Para no confundirse, en la forma más tradicional de escribir funciones a la expresión a la derecha del igual no le falta ningún parámetro:

`siguiente x = x + 1`

Se ve claramente que x+1 es una expresión que denota un valor numérico y no una función, o dicho de otra manera "no le faltan parámetros". En cambio sí le faltarían si uno pusiera por ejemplo (+1).

La definición anterior es totalmente equivalente a:

`siguiente = (+1)`
