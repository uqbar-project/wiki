Entendemos por *inferencia de tipos* la capacidad que tienen algunos lenguajes con [chequeo fuerte de tipos](chequeo-fuerte-de-tipos.html) para calcular el tipo de una expresión, función o variable sin necesidad de contar con [anotaciones de tipo](anotaciones-de-tipo.html).

¿La inferencia de tipos es una característica del paradigma funcional?
----------------------------------------------------------------------

Si es un concepto que fue desarrollado primero en el contexto del paradigma funcional e históricamente se lo ha encontraro principalmente asociado a lenguajes funcionales; la idea de inferencia no está limitada este tipo de lenguajes y cada vez más está siendo utilizada en todo tipo de lenguajes.

Algunos lenguajes que poseen formas de inferencia de tipos son: Ada, BitC, Boo, C\# 3.0, Cayenne, Clean, Cobra, D, Delphi, Epigram, F\#, Haskell, haXe, JavaFX Script, ML, Mythryl, Nemerle, OCaml, Oxygene, Scala.

Por otro lado, la inferencia es más simple en los lenguajes que no permiten realizar asignaciones; y eso hace que sea más fácil de implementar en los lenguajes declarativos puros como Haskell.

Ejemplos
--------

-   [Cálculo del tipo de una función en Haskell](calculo-del-tipo-de-una-funcion-en-haskell.html)

Un problema de inferencia en Haskell
------------------------------------

Estos días varios estuvieron experimentando un error como este:

``    Ambiguous type variable `a' in the constraint: ``
``      `Eq a' ``
``        arising from a use of `sinDuplicados' at integrador.hs:38:14-26 ``
`   Possible cause: the monomorphism restriction applied to the following:`
`     repetidos :: [a] -> [a] (bound at integrador.hs:38:0)`

no?

Bueno, ese error se debe a que Haskell no puede, en esa función en particular por la que estalla, inferir el tipo.

Soluciones posibles:

1) En vez de escribir nuestra función como

`  f = g.h`

hacer

`  f x = (g.h) x`

2) explicitar el tipo de nuestra función, como por ejemplo:

`   sarasa :: (Eq a) => [a] -> [a]`
`   sarasa = algo . otroAlgo`
