Patrones con sinónimos
----------------------

Un sinónimo, o sea necesito por un lado el patrón para tomar algunos de sus componentes y por otro todo junto para la llamada recursiva, tal vez si lo escribo bien se entienda más:

`ordenada [_] = True`
`ordenada (x1:x2:xs) = x1 > x2 && ordenada x2:xs`

Puede mejorarse sutilmente con un sinónimo.

`ordenada [_] = True`
`ordenada (x1:xs'@(x2:_)) = x1 > x2 && ordenada xs`

Otro ejemplo, entre dos complejos representados con tuplas, obtener el que tiene mayor parte real:

`mayorParteReal (a@(a1, _)) (b@(b1, _))`
` | a1 > b1 = a`
` | otherwise = b`
