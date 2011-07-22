Muchas veces mencionamos los conceptos de declaratividad y expresividad, pero ¿cuando corresponde hablar de uno o del otro? ¿Y como se relacionan con otros conceptos vistos en la materia? Veamoslo con algunos ejemplos:

  
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

Así como pasa con orden superior y declaratividad, pasa con la idea de polimorfismo y declaratividad. Que se use orden superior o polimorfismo no asegura un mayor grado de declaratividad en la solución, depende de cómo se hayan utilizado.

Otra cualidad que remarcamos en las soluciones es la expresividad, decimos que una solución A es más expresiva que una solución B si la solución A se entiende más rápido/es más fácil de leer que la solución B.

Claramente la idea de expresividad puede verse de forma subjetiva.

Volviendo a las soluciones para sumar una lista de números

-   Solución 3

`  sumar [] = 0`
`  sumar (x:xs) = x + sumar xs`

Alguien puede decir que la solución 1 es más expresiva que la solución 2 (porque le gusta el foldl y lo entiende) pero en la primer clase de funcional si te muestran (sin explicar) entre la solución 1 y la solución 2 es muy probable que la solución 2 pasaría a ser la más expresiva. En contraste, para alguna persona retorcida la solución 3 puede ser la más expresiva ....

A lo que vamos con esto es que, la idea de declaratividad debería ser más objetiva que la idea de expresividad.

De todas formas, con la debida justificación y relacionándolo con el ejercicio del final, se puede hablar de ambos conceptos.

Tambén se puede hablar de delegación, sin necesidad de que haya polimorfismo u orden superior.

` Tanque >> dispararA: otroTanque`
`    | unMisil |`
`    unMisil := misiles anyOne.`
`    misiles remove: unMisil.`
`    danioTotal := unMisil cuantoDañoPara: otroTanque.`
`    otroTanque coraza > danioTotal ifTrue: `
`         [ otroTanque coraza: otroTanque coraza - danioTotal ]`
`         [ otroTanque coraza: 0 ].`

Fijense que en el ejemplo del tanque si asumimos que hay muchos misiles (instancias de diferentes clases), desde el tanque las tratamos polimórficamente envíandoles el mensaje \#cuantoDañoPara:, pero no se si delegamos lo suficiente.

Mira esta otra solución

` Tanque >> dispararA: otroTanque`
`    self descargarMisil dañarA: otroTanque`

` Tanque >> descargarMisil`
`   "El remove: devuelve el parámetro"`
`   ^misiles remove: misiles anyOne`

` Tanque >> recibirDaño: cant`
`   self coraza: (self coraza - cant) max: 0`

` Misil >> dañarA: otroTanque`
`   "Asumimos que Misil es la superclase de todos las otras clases de misiles"`
`   otroTanque recibirDaño: (self cuantoDañoPara: otroTanque)`

También se puede hablar de extensibilidad, si el día de mañana se agrega un nuevo misil (el misil Gandhi) que en vez de sacar coraza le hace un cariñito al otro tanque

Con la segunda solución eso se agrega bastante fácil

` MisilGandhi >> dañarA: otroTanque`
`   otroTanque recibirCariñito`

La segunda solución es más extensible.

Y se puede volver a hablar de expresividad, porque que el método se llama \#dañarA: y me da la sensación de que siempre le saca coraza ... tal vez deberíamos buscar un mejor nombre.... y así....
