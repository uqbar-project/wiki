---
layout: article
title: Fold
---

Existe una familia de funciones en Haskell para modelar un algoritmo que permite procesar una estructura de datos para construir un valor, a esta idea le decimos foldear (derivado del inglés, "to fold") o reducir.

¿Cómo sumo los elementos de una lista? ¿Cómo multiplico los elementos de una lista? ¿Cómo concateno una lista de palabras?

`sum [] = 0 `
`sum (x:xs) = x + sum xs `

`prod [] = 1 `
`prod (x:xs) = x * prod xs `

`concatenar [] = [] `
`concatenar (x:xs) = x ++ concatenar xs`

O bien si usamos los operadores de forma infija…

`sum [] = 0 `
`sum (x:xs) = (+) x (sum xs) `

`prod [] = 1 `
`prod (x:xs) = (*) x (prod xs) `

`concatenar [] = [] `
`concatenar (x:xs) = (++) x (concatenar xs)`

Si vemos las diferencias entre estas funciones notamos que sólo cambia la operación a realizar y el valor inicial, por ende si queremos hacer una función que nos generalice el algoritmo tenemos que recibirlos como parámetro.

En términos generales… ¿cómo la paso a una función de orden superior? Necesitamos • una función que opera con dos parámetros • un valor inicial • una lista

`foldr :: (a -> b -> b) -> b -> [a] -> b `

Caso base: si la lista es vacía, devuelvo el valor inicial.

`foldr f valorInicial [] = valorInicial `

Si la lista no es vacía, ¿qué voy a tener que hacer? A partir de alguna de las funciones podemos trasladar:

`concatenar (x:xs) = (++) x (concatenar xs) `
`foldr f valorInicial (x:xs) = f x (foldr f valorInicial xs)`

Sumando con foldr
-----------------

`sum = foldr (+) 0 `

Para sumar una lista hay que aplicar la suma elemento por elemento arrancando de 0. Como es difícil de asimilar de golpe, vamos a hacer el seguimiento de un caso:

`> sum [2, 3, 5] `
`= foldr (+) [2, 3, 5] `
`= (+ 2) (foldr (+) 0 [3, 5]) `
`= (+ 2) ((+ 3) (foldr (+) 0 [5])) `
`= (+ 2) ((+ 3) ((+ 5) (foldr (+) 0 []))) `
`= (+ 2) ((+ 3) ((+ 5) 0)) <- caso base `
`= (+ 2) ((+ 3 5)) `
`= (+ 2) 8 `
`= 10`

Si revisan el Prelude, van a encontrar otra función similar:

`foldl f z [] = z `
`foldl f z (x:xs) = foldl f (f z x) xs `

foldl vs foldr
--------------

¿Cuál es la diferencia? Revisemos los tipos:

`foldl :: (a -> b -> a) -> a -> [b] -> a `
`foldr :: (a -> b -> b) -> b -> [a] -> b `

Foldr trabaja asociando a derecha la función f, mientras que foldl trabaja asociando a izquierda la función f. El seguimiento de la sumatoria definida en base a foldl puede ayudar a entender cómo realiza la operatoria:

`sum = foldl (+) 0 `
`> sum [2, 3, 5] `
`= foldl (+) 0 [2, 3, 5] `
`= foldl (+) ((+) 0 2) [3, 5] `
`= foldl (+) ((+) 2 3) [5] `
`= foldl (+) ((+) 5 5) []`
`= foldl (+) 10 [] `
`= 10 <- caso base`

Y aquí vemos que el valor inicial del caso base es en realidad el resultado final. Para comparar la forma de trabajar de las dos funciones pueden realizar consultas de este estilo en su intérprete de Haskell (show es una función que retorna la representación de el valor recibido como string):

`> foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])`
`"(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+(11+(12+(13+0)))))))))))))"`
`> foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])`
`"(((((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)+11)+12)+13)"`

Otras variantes de fold son foldl1 y foldr1 que trabajan de forma análoga a las anteriores sólo que toman como valor inicial al primer elemento de la lista. Eso es útil para los casos en los que la lista no debería estar vacía y/o no hay un valor adecuado para parametrizar. Veamos un ejemplo:

`maximum' = foldr1 max`
