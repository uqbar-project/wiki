Una función recursiva es aquella que en su definición se invoca a sí misma. La misma por lo general cuenta con una definición recursiva y al menos un caso base que corta la recursividad.

Recursividad sin listas
-----------------------

Un ejemplo clásico de recursividad es \[<http://es.wikipedia.org/wiki/Sucesi%C3%B3n_de_Fibonacci>| Fibonacci\].

`fibonacci 0 = 0`
`fibonacci 1 = 1`
`fibonacci n = fibonacci (n-1) + fibonacci (n-2)`

Esta función tiene dos casos base para el 0 y el 1 y para todos los otros números una definición recursiva.

Recursividad con listas
-----------------------

Siendo las listas estructuras recursivas (compuestas por una cabeza y una cola que a su vez es una lista) la forma más natural para trabajar con ellas es recursivamente. La lista \[1,2,3\] puede también escribirse como 1:2:3:\[\] (donde el : es la función usada para armar listas y puede ser usada convenientemente para [pattern matching](pattern-matching-en-haskell-con-listas.html))

Un ejemplo fácil es el length.

length \[\] = 0 length (x:xs) = 1 + length xs

Usamos el patrón de lista vacía \[\] para el caso base, ya que la segunda definición indefectiblemente nos llevará a ella, y el patrón de cabeza y cola para poder avanzar de a un paso por la estructura y poder procesarla fácilmente.
