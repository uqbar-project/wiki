El desafío
----------

Se trata de escribir la función append en Haskell a partir de la función foldr. Hay tres versiones:

1.  append = ... expresion ...
      
    donde en la expresión dice foldr en algún lado, y realmente se usa. Se puede hacer usando únicamente tres funciones, foldr y dos más.

2.  append = foldr f a
      
    donde f puede ser una expresión lambda

3.  append = foldr f a
      
    donde f no puede ser una expresión lambda

Soluciones
----------

by Nicolás Perez Santoro.

### Primero

`append:: [a] -> [a] -> [a]`
`foldr :: (a -> b -> b) -> b -> [a] -> b`

Si defino append = foldr f, si tengo la lista \[1,2,3\] \`append\` \[4,5,6\] el foldr f me va a hacer

`` 4 `f` (5 `f` (6 `f` [1,2,3])) ``

Pero si pudiera hacer un flip de los argumentos, sería

`` 1 `f` (2 `f` (3 `f` [4,5,6])) ``

Entonces la primer versión del append es

`append = flip (foldr (:))`

### Segundo

La segunda versión tiene la forma

`append = foldr f a`

Pero miro el tipo de foldr

`foldr :: (a -> b -> b) -> b -> [a] -> b`

Okay, y el tipo de foldr f a ?

`foldr f a :: [a] -> b`

Pero yo quiero que foldr f a :: \[a\] -&gt; \[a\] -&gt; \[a\], que es el tipo de append. entonces b es (\[a\]-&gt;\[a\]). Entonces foldr en este caso queda instanciado así

`foldr :: (a -> ([a] -> [a]) -> ([a] -> [a])) -> ([a] -> [a]) -> [a] -> ([a] -> [a])`

Uh que quilombo juajauj. quiero que foldr, a partir del valor inicial, que va a ser una funcion, y la primer lista me devuelva una función que dada la segunda lista del append, me de la concatenacion de las listas. rebuscado eh. Pero pienso, si la lista de la izquierda está vacía, entonces foldr me tiene que devolver la función id. entonces digo

`append2 = foldr f id`

Pero que es f? tiene que ser una función que dado un elemento y una funcion que dada una lista devuelve otra lista, devuelva otra funcion que dada una lista devuelva la lista que devolvería la función esta, pero con el elemento al principio.

`f :: a -> ([a] -> [a]) -> ([a] -> [a])`
`f e g = \ l -> e : g l`

Y esta es la segunda versión. PUFFFF, QUE QUILOMBO.

### Tercero

La tercer versión claramente tiene que ser como la segunda, pero sin lambda expresions ....!!! Si reduzco un poco, llego a esto

`f' e = (.) (e:)`

Donde

`(.) :: ([a] -> [a]) -> ([a] -> [a]) -> ([a] -> [a])`
`(e:) :: ([a] -> [a])`

pero

`(:) :: (a -> [a] -> [a]) y f' :: ([a] -> [a]) -> ([a] -> [a])`

Los tipos me dicen que componga! pero como compongo?

`(:) :: a -> ([a] -> [a])`
`(.) :: ([a] -> [a]) -> ([a] -> [a]) -> ([a] -> [a])`

entonces

`(.) . (:)  :: a -> ([a] -> [a]) -> ([a] -> [a])`

(uy que quilombo)

entonces

`append3 = foldr ((.) . (:)) id`

MORALEJA:LOS TIPOS SON MIS AMIGOS.
COROLARIO:LOS PROGRAMADORES FUNCIONALES NOS DIVERTIMOS BARATO EH  


