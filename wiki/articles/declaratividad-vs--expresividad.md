Muchas veces mencionamos los conceptos de [Declaratividad](declaratividad.html) y Expresividad, pero ¿cuando corresponde hablar de uno o del otro? ¿Y como se relacionan con otros conceptos vistos en la materia? Veamoslo con algunos ejemplos:

  
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
