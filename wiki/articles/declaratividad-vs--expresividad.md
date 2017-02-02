---
layout: article
title: Declaratividad vs  expresividad
---

Muchas veces mencionamos los conceptos de [Declaratividad](declaratividad.html) y [Expresividad](expresividad.html), pero ¿cuando corresponde hablar de uno o del otro? ¿Y como se relacionan con otros conceptos vistos en la materia? Veamoslo con algunos ejemplos:

  
"sumar una lista de números ns"

-   Solución 1 (utiliza el concepto de orden superior):

`    foldl (+) 0 ns`

-   Solución 2 (no utiliza el concepto de orden superior):

`    sum ns`

La solución 2 es más declarativa que la solución 1 ya que en la solución 1 hay que saber:

  
que se suma con la función (+)

cómo se define el valor inicial del acumulador (0)

En la solución 2 estos detalles no existen

Entonces:

-   Siempre que se hable de más o menos declarativo comparen cosas, no digan más declarativo al aire
-   Una solución A es más declarativa que una solución B si:

  
1. A tiene menos detalles algorítmicos que B

2. Esto significa que la noción de algorítmo/secuencia de pasos/definición del "cómo lo hace" sea menor en A que en B

Así como pasa con orden superior y declaratividad, pasa con la idea de polimorfismo y declaratividad.

Otra cualidad que remarcamos en las soluciones es la expresividad, **decimos que una solución A es más expresiva que una solución B si la solución A se entiende más rápido/es más fácil de leer que la solución B.**

Claramente la idea de expresividad puede verse de forma subjetiva.

Volviendo a las soluciones para sumar una lista de números

-   Solución 3

`  sumar [] = 0`
`  sumar (x:xs) = x + sumar xs`

Alguien puede decir que la solución 1 es más expresiva que la solución 2 (porque le gusta el foldl y lo entiende) pero en la primer clase de funcional si te muestran (sin explicar) entre la solución 1 y la solución 2 es muy probable que la solución 2 pasaría a ser la más expresiva. En contraste, para alguna persona retorcida la solución 3 puede ser la más expresiva ....

A lo que vamos con esto es que, la idea de declaratividad debería ser más objetiva que la idea de expresividad.

De todas formas, con la debida justificación y relacionándolo con el ejercicio del final, se puede hablar de ambos conceptos. Por lo general, las soluciones más declarativas resultan a su vez más expresivas, pero dejando las subjetividades a un lado, un código imperativo bien hecho puede resultar más legible que uno declarativo con nombres de operaciones y variables mal puestos.

Declaratividad, expresividad y abstracción
------------------------------------------

Es importante no confundir los términos de expresividad, abstracción y declaratividad. Aunque están muy relacionados entre sí, son tres conceptos diferentes, que bien pueden darse por separado.

Como decíamos antes, la relación que con más frecuencia vamos a encontrar es que un código declarativo tiende a ser más expresivo que uno imperativo, ya que puedo leer directamente de qué se trata el problema, en lugar de deducir qué es lo que un algoritmo está tratando de resolver.

Ejemplo en pseudocódigo:

`Integer cuantosPares(Array ns){`
` Integer i;`
` Integer acum = 0;`
`  for(i = 0 ; i < longitudDelArray(ns) ; i = i + 1){`
`   if (ns[i] mod 2 == 0) {`
`    acum = acum + 1;`
`   }`
`  }`
` return acum;`
`}`

Esta función de por sí representa una [abstracción](abstraccion.html): es una operación que puedo utilizar cada vez que necesite saber la cantidad de números pares hay en el array que se recibe por parámetro. Gracias a que tiene un nombre apropiado (detalle que aumenta la expresividad), podría no tener que leer el código para saber qué es lo que hace.

Leyendo el cuerpo de la función, podemos tratar de encontrar otra abstracción más. En el contexto de la función cuantosPares, ¿qué significa este fragmento?

`if (ns[i] mod 2 == 0) {`
` acum = acum + 1;`
`}`

Podemos entender que se está evaluando si ns\[i\] (el elemento del array que está en la posición i) es múltiplo de 2, y si esto es cierto contamos un par más. Una abstracción podría ser una función que evalúe si un número (en este caso ns\[i\]) es múltiplo de otro (en este caso 2). El código cambiaría a:

`if(esMultiploDe(ns[i],2)) {`
` acum = acum + 1;`
`}`

Al encontrar una abstracción y ponerle nombre, hace que mi código quede más expresivo. Pero recordemos que hacíamos esto para saber si ns\[i\] era par. Cuando evaluamos la función esMultiploDe con 2 como segundo argumento, estamos justamente preguntando si ns\[i\] es par.

Teniendo en cuenta esto, podríamos tener directamente la abstracción esPar, i.e. una función que recibe un solo argumento (un número) y me dice si ese número es par o no. Por otro lado, si estamos contando pares, la variable acum bien podría llamarse cantidadDePares. Entonces el código podría quedar así:

`if(esPar(ns[i])) {`
` cantidadDePares = cantidadDePares + 1;`
`}`

Podemos ver que un componente esencial de la expresividad puede ser el elegir buenos nombres para los elementos de mi código (variables, funciones, procedimientos, etc).

Haber encontrado estas abstracciones ayudó a que el código quede más expresivo, ya que una se entiende más rápido lo que hace esPar(ns\[i\]) en comparación con esMultiploDe(ns\[i\],2) o con ns\[i\] mod 2 == 0.

Ahora el código expresa que un número tiene que ser par en vez de exponer el algoritmo o las operaciones matemáticas necesarias para saber si el módulo de la división por 2 es 0. Esta última solución está más cerca de qué quiero resolver en vez de cómo pretendo resolverlo.
