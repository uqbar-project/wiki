Una función recursiva es aquella que en su definición se invoca a sí misma. La misma por lo general cuenta con una definición recursiva y al menos un caso base que corta la recursividad.

Recursividad sin listas
-----------------------

Un ejemplo clásico de recursividad es \[<http://es.wikipedia.org/wiki/Sucesi%C3%B3n_de_Fibonacci>| Fibonacci\].

`fibonacci 0 = 0`
`fibonacci 1 = 1`
`fibonacci n = fibonacci (n-1) + fibonacci (n-2)`

Esta función tiene dos casos base para el 0 y el 1 y para todos los otros números una definición recursiva.

Para pensar: qué pasa si la consulta realizada es

`fibonacci (-1)`

Dado que la tercer definición admite números negativos va a entrar en un loop infinito. Como no es correcto usar fibonacci con números negativos, podríamos mejorar nuestra definición restringiendo el dominio de la función usando [guardas](funciones-por-partes.html) de esta forma:

`fibonacci 0 = 0`
`fibonacci 1 = 1`
`fibonacci n | n >= 2 = fibonacci (n-1) + fibonacci (n-2)`

Ahora el resultado de aplicar fibonacci con un número negativo sería un error ya que no hay ninguna definición válida para el valor indicado :)

Recursividad con listas
-----------------------

Siendo las listas estructuras recursivas (compuestas por una cabeza y una cola que a su vez es una lista) la forma más natural para trabajar con ellas es recursivamente. La lista \[1,2,3\] puede también escribirse como 1:2:3:\[\] (donde el : es la función usada para armar listas y puede ser usada convenientemente para [pattern matching](pattern-matching-en-haskell-con-listas.html))

Un ejemplo fácil es el length.

`length [] = 0`
`length (x:xs) = 1 + length xs`

Usamos el patrón de lista vacía \[\] para el caso base, ya que la segunda definición indefectiblemente nos llevará a ella, y el patrón de cabeza y cola para poder avanzar de a un paso por la estructura y poder procesarla fácilmente. En este caso particular no importa el orden de las definiciones, ya que la lista con al menos un elemento no matchea con la lista vacía y viceversa.

Loops infinitos
---------------

Analicemos la siguiente función:

`muchosDe n = n:(muchosDe n)`

Si consultamos en el intérprete

`> muchosDe 5`

Esta función no podría terminar nunca, ya que no hay ningún punto en el que se corte la recursividad. Sin embargo, lo que sí podríamos hacer con esta función es usarla en un contexto que acote la ejecución gracias a la [evaluación perezosa](estrategias-de-evaluacion-lazy-evaluation.html). Veamos el siguiente ejemplo:

`> (sum.take 10.muchosDe) 5`
`50`
