La posibilidad de definir funciones utilizando aplicación parcial en la propia definición (lo cual se conoce como notación **point-free**) trae aparejado a veces la dificultad para entender cuántos parámetros espera esa función que se está definiendo.

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

Pros y Cons de la notación point-free
-------------------------------------

A favor de usar la notación point-free es que nos fuerza a dejar de pensar en función de la aplicación y los valores con los que estamos acostumbrados a trabajar, teniendo que pensar en definir funciones como equivalencia de otras funciones combinadas. El paradigma funcional se basa en la idea de combinar funciones, ellas son las divas de esta forma de pensar la programación, por eso cuando definimos funciones como composición de otras funciones y/o al aplicación parcial de las mismas con estilo point-free, el código resultante es más limpio :)

La notación point-free puede llevar a ofuscar el código cuando se usa malintencionadamente. Al combinar funciones puede que se vuelva más complicado inferir mentalmente el tipo de una expresión ya que cuesta más deducir la cantidad de parámetros necesarios para que la función definida se reduzca a un valor concreto mediante la aplicación.

Otro problema que podría surgir dada una función compleja escrita de esta forma es que al intentar introducir un cambio menor de funcionalidad en la misma haya que hacer un cambio radical en el código, sin embargo para el nivel de paradigmas esto no debería suceder.
