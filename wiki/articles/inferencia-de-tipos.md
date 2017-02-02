---
layout: article
title: Inferencia de tipos
---

Entendemos por *inferencia de tipos* la capacidad que tienen algunos lenguajes con [chequeo estático de tipos](esquemas-de-tipado.html) para calcular el tipo de una expresión, función o variable sin necesidad de contar con anotaciones de tipo como sí se necesitan en C y Java por ejemplo.

La clave de este proceso es la regla por la cual se obtiene el tipo de la aplicación de una función:

`f :: AB  ->  e :: A`
`-------------------`
`      f e :: B`

En términos más mundanos, lo que la regla dice es

1.  si f es una función cuyo dominio es A e imagen es B y,
2.  si e es una expresión de tipo A
3.  entonces, el tipo de f e (usamos a e como argumento de f) es B

Ejemplo, el tipo de not False se puede inferir con esta regla

1.  si not es una función cuyo dominio es Bool e imagen es Bool y,
2.  si False es una expresión de tipo Bool
3.  entonces, el tipo de not False es Bool

Otro ejemplo, si queremos saber de que tipo es la expresión not 3 vemos que no tiene un tipo siguiendo la regla que especificamos anteriormente, porque requeriría que 3 sea de tipo Bool pero 3 no es un booleano.

Expresiones como not 3, 2 + True, length (4,3), fst \[1,2\], etc. no tienen un tipo y por lo tanto no son expresiones válidas en Haskell. Analicemos el siguiente caso:

`funcionLoca x`
`  | x > 0 = 1`
`  | otherwise = True`

Como por una rama retornamos un Int (1 :: Int) y por la otra retornamos un Bool (True :: Bool) no se puede obtener la imagen de la función funcionLoca, ergo la funcionLoca no se puede escribir en Haskell porque no tiene un tipo.

Sin embargo, que una expresión tenga tipo no significa que sea una expresión libre de errores.

`head :: [ a ] -> a`
`head [] :: a`

Pero a pesar de esto, si evaluamos la expresión

`> head []`
`Error`

Como la inferencia de tipos es un proceso ANTERIOR a la evaluación, los programas que hacemos en Haskell son [Type Safe](http://en.wikipedia.org/wiki/Type_safety)

¿La inferencia de tipos es una característica del paradigma funcional?
----------------------------------------------------------------------

Sí, es un concepto que fue desarrollado primero en el contexto del paradigma funcional e históricamente se lo ha encontraro principalmente asociado a lenguajes funcionales; sin embargo la idea de inferencia no está limitada este tipo de lenguajes y cada vez más está siendo utilizada en todo tipo de lenguajes.

Algunos lenguajes que poseen formas de inferencia de tipos son: Ada, BitC, Boo, C\# 3.0, Cayenne, Clean, Cobra, D, Delphi, Epigram, F\#, Haskell, haXe, JavaFX Script, ML, Mythryl, Nemerle, OCaml, Oxygene, Scala.

Por otro lado, la inferencia es más simple en los lenguajes que no permiten realizar asignaciones; y eso hace que sea más fácil de implementar en los lenguajes declarativos puros como Haskell.

Ejemplos
--------

Ver [Cálculo del tipo de una función en Haskell](calculo-del-tipo-de-una-funcion-en-haskell.html)

Recuerden que en Haskell ustedes pueden obtener el tipo de cualquier expresión escribiendo :t (type) en el interprete

`> :t not`
`not :: Bool -> Bool`

`> :t not False`
`not False :: Bool`

`> :t not 3`
`Error`

Problemas de inferencia en Haskell
----------------------------------

Si bien Haskell puede inferir correctamente el tipo de las funciones que definimos casi siempre, existen casos en los cuales necesita un poco de ayuda de parte del programador. En caso de que eso pase, conviene explicitar el tipo de nuestra función además de su definición en el .hs.

Por ejemplo, si estamos haciendo una función f que recibe una lista de a y retorna una lista de a, podemos hacer:

`f :: [a] -> [a]`
`f lista = `<lo que sea que hay que hacer con la lista para retornar otra>
