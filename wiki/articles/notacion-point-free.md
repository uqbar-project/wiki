A veces, como consecuencia de definir solamente en términos de [Aplicación Parcial](aplicacion-parcial.html) y/o [Composición](composicion.html) de funciones, escribir explícitamente los parámetros de la misma se vuelve redundante.

Por ejemplo, se podría escribir

`` esMultiploDe a b = ((==0).( `mod` a)) b  ``

Sin embargo, lo que está dentro del paréntesis principal ya es una función que hace lo que queremos, con el tipo adecuado. Por lo que puede reescribirse de la siguiente manera:

`` esMultiploDe a = (==0).( `mod` a) ``

Lo cual se conoce como notación **point-free**, la cual es interesante porque pone nos permite concentrarnos en la combinación de las funciones en lugar del pasaje de paramétro, subiendo así nuestro nivel de abstracción.

Al principio puede generar cierta confusión sobre la pregunta ¿cuantos parámetros recibe la función?. Así que analicemos algunos casos concretos.

Una reglita básica
------------------

La regla es que una función como esa tiene tantos parámetros como la suma entre:

-   Los parámetros que aparecen a la izquierda del igual.
-   Los parámetros que "le faltan" a la expresión a la derecha del igual.

Analizando el caso anterior vemos:

-   Un parámetro a la izquierda del igual: a. (Esta parte es la más directa.)
-   La expresión de la derecha es una composición y por lo tanto una función a-&gt;b entonces le falta un parámetro.
-   En total la función recibe dos parámetros.

Analizando un poco más los tipos
--------------------------------

Para no confundirse, en la forma más tradicional de escribir funciones a la expresión a la derecha del igual no le falta ningún parámetro:

`siguiente x = x + 1`

Se ve claramente que x+1 es una expresión que denota un valor numérico y no una función, o dicho de otra manera "no le faltan parámetros". En cambio sí le faltarían si uno pusiera por ejemplo (+1).

La definición anterior es totalmente equivalente a:

`siguiente = (+1)`

Al aplicar parcialmente la función (+) :: Num a =&gt; a -&gt; a -&gt; a obtenemos otra función que es del tipo Num a =&gt; a -&gt; a. Si siguiente es igual a la función (+1), entonces su tipo es el mismo que el de (+1).

En general lo más común es ver funciones que sólo les falta el último parámetro, como en el caso de esMultiploDe, ya que está definida en función de una composición. No sería correcto definirla de esta forma a pesar de que ambos parámetros vayan en ese orden "aplicados al final":

`esMultiploDe = (==0).flip mod`

Como no coincide la imagen de la función flip mod

`flip mod :: Integral c => c -> `**`c` `->` `c`**

con el dominio de (==0)

`(==0) :: (Eq a, Num a) => `**`a`**` -> Bool`

La composición de estas dos funciones no es correcta (de hecho esa expresión tipa, pero no lo van a poder usar porque el tipo de esMultiploDe requeriría un único argumento que sea a la vez una función y un número equiparable... y como que no tiene mucho sentido)

Este otro ejemplo sí sería correcto:

`resto = flip mod`

El tipo de la función resto es el mismo que el de la función flip mod que ya mostramos antes.

Pros y Cons de la notación point-free
-------------------------------------

A favor de usar la notación point-free es que nos fuerza a dejar de pensar en función de la aplicación y los valores con los que estamos acostumbrados a trabajar, teniendo que pensar en definir funciones como equivalencia de otras funciones combinadas. El paradigma funcional se basa en la idea de combinar funciones, ellas son las divas de esta forma de pensar la programación, por eso cuando definimos funciones como composición de otras funciones y/o a la aplicación parcial de las mismas con estilo point-free, el código resultante es más limpio :)

La notación point-free puede llevar a ofuscar el código cuando se usa malintencionadamente. Al combinar funciones puede que se vuelva más complicado inferir mentalmente el tipo de una expresión ya que cuesta más deducir la cantidad de parámetros necesarios para que la función definida se reduzca a un valor concreto mediante la aplicación.

Otro problema que podría surgir dada una función compleja escrita de esta forma es que al intentar introducir un cambio menor de funcionalidad en la misma haya que hacer un cambio radical en el código, sin embargo para el nivel de paradigmas esto no debería suceder.
