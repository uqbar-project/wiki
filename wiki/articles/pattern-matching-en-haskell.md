Ejemplos de matcheo
-------------------

### Con tuplas

Si tengo los valores

`v1 = (2,5)`
`v2 = (3,5)`
`v3 = (8,5)`

El patrón `(x,5)` matchea con los tres valores.

También el patrón `(x,y)` matchea con los tres valores.

También el patrón `(_,5)` matchea con los tres valores.

También el patrón `(_,_)` matchea con los tres valores.

También el patrón `unaTupla` matchea con los tres valores.

Ahora, si tengo una lista

`lista1 = [(2,5),(5,3),(3,3)]`
`lista2 = [(2,5)]`

El patrón `(x,5)` no matchea con `lista1` ni con `lista2`. ¡Es una tupla, no una lista! Antes de tratar de matchear, Haskell nos va a tirar un error de tipo.

El patrón `(x:xs)` matchea con `lista1`, siendo `x` `=` `(2,5)` y `xs` `=` `[(5,3),(3,3)]` y matchea con `lista2`, siendo `x` `=` `(2,5)` y `xs` `=` `[]`

El patrón `[x]` no matchea con `lista1` pero si matchea con `lista2`, siendo `x` `=` `(2,5)`

El patrón `(x:_)` matchea con `lista1`, siendo `x` `=` `(2,5)` y matchea con `lista2`, siendo `x` `=` `(2,5)`

El patrón `unaTupla` matchea con `lista1` siendo `unaTupla` `=` `[(2,5),(5,3),(3,3)]` y matchea con `lista2` siendo `unaTupla` `=` `[(2,5)]`

### Con data

(En algunos cursos no se da data, para más información ver acá: [Data: Definiendo nuestros tipos en Haskell](data--definiendo-nuestros-tipos-en-haskell.html)) Suponiendo que tenés

`data Coordenada = Coord Int Int`

(el tipo es Coordenada y el constructor es Coord)

Donde se puede aplicar la misma idea

`lista1 = [Coord 2 5,Coord 5 3,Coord 3 3]`
`lista2 = [Coord 2 5]`

El patrón `(Coord` `x` `5)` no matchea con `lista1` ni con `lista2`. ¡Es una Coordenada, no una lista! Antes de tratar de matchear, Haskell nos va a tirar un error de tipo.

El patrón `(Coord` `x` `y:restoCoords)` matchea con `lista1`, siendo `x` `=` `2`, `y` `=` `5` y `restoCoords` `=` `[Coord` `5` `3,Coord` `3` `3]` y matchea con `lista2`, siendo `x` `=` `2`, `y` `=` `5` y `restoCoords` `=` `[]`

El patrón `[x]` no matchea con `lista1` pero si matchea con `lista2`, siendo `x` `=` `2` y `y` `=` `5`

El patrón `(x:_)` matchea con `lista1` y matchea con `lista2` siendo `x` `=` `Coord` `2` `5`

El patrón `(x:y:_)` matchea con `lista1` siendo `x` `=` `Coord` `2` `5` y `y` `=` `Coord` `5` `3` pero no matchea con `lista2`

El patrón `unaTupla` matchea con `lista1` y matchea con `lista2`

Patrones con sinónimos
----------------------

Un sinónimo, o sea necesito por un lado el patrón para tomar algunos de sus componentes y por otro todo junto para la llamada recursiva, tal vez si lo escribo bien se entienda más:

`ordenada [_] = True`
`ordenada (x1:x2:xs) = x1 > x2 && ordenada x2:xs`

Puede mejorarse sutilmente con un sinónimo.

`ordenada [_] = True`
`ordenada (x1:xs@(x2:_)) = x1 > x2 && ordenada xs`

Otro ejemplo, entre dos complejos representados con tuplas, obtener el que tiene mayor parte real:

`mayorParteReal (a@(a1, _)) (b@(b1, _))`
` | a1 > b1 = a`
` | otherwise = b`
