Una excepción o error es la indicación de un problema que ocurre durante la ejecución de un programa porque alguna determinada operación no pudo realizarse. El manejo de excepciones permite al usuario crear aplicaciones tolerantes a fallas y robustos (resistentes a errores) para controlar estas excepciones y que pueda seguir ejecutando el programa sin verse afectado por el problema.

La principal particularidad de los errores es que cortan el flujo de ejecución hasta que alguien se encargue de resolverlo. De esta forma si a recibe el mensaje msj1 que internamente manda self msj2 que a su vez manda self msj3 y el mismo no puede realizarse, el error se propaga hacia atrás por la cadena de mensajes enviados hasta que el mismo sea atrapado y se pueda continuar ejecutando el programa con normalidad.

Bugs vs Errores de usuario
--------------------------

Algunos errores surgen por un bug en el programa, por ejemplo si un objeto no entiende un mensaje la forma de resolverlo es modificar el código para que o bien lo entienda o el mismo no le llegue dependiendo de si debería o no entenderlo. Por ejemplo, si a un Set le pedimos el primer elemento tira un error porque no es una colección ordenada, por ende no debe responder al mensaje first como sí lo hace una colección ordenada.

Otros errores surgen del uso del programa, ya que pueden darse situaciones que llevan a que un objeto no pueda realizar lo que se le pide. Por ejemplo, si a una colección vacía el mandamos el mensaje anyOne tira un error porque no tiene forma de resolver el problema.

Lanzando errores
----------------

En Smalltalk la forma más fácil de lanzar un error es usando el mensaje error: que está definido en Object y por ende todos los objetos entienden. Por ejemplo:

`Golondrina >> vola: unosKms`
`  self energia < unosKms`
`    ifTrue: [self error: 'No me da la nafta'].`
`  self energia: self energia - unosKms `

En el ejemplo vemos que si la energía de la golondrina es menos a la cantidad de kilómetros pasados por parámetro, la operación no debería realizarse porque quedaría con energía negativa. Para evitar que eso pase se lanza el error con una descripción simpática para que el usuario o el desarrollador (dependiendo de si debería o no llegarse a esa situación) entienda qué fue lo que pasó. Lo interesante es que la línea que modifica la energía sólo llega a ejecutarse si energia &gt;= unosKms.

Otra forma de lanzar errores (que viene asociada a la forma de atraparlos) es con mensaje que entienden las clases que heredan de Exception llamado signal: Esta expresión es equivalente a la que vimos antes:

`Error signal: 'No me da la nafta'.`

También es posible instanciar una clase de esa jerarquía, como ser Error, y enviarle signal: o signal a la instancia más adelante. Por ejemplo:

`unError := Error new messageText: 'Oops!'`
`"acá hacemos cosas"`
`unError signal.`

¿Cómo evitar que se rompa todo ante situaciones excepcionales?
--------------------------------------------------------------

Algunos errores pueden evitarse realizando validaciones previas, pero no siempre es posible o deseable usar este enfoque. Entonces, una vez que se produce el error tenemos que tener una forma de recuperarnos del mismo para que el programa no termine con excepción.

Lo que deberíamos hacer en aquellos lugares en donde sabemos qué hacer ante un problema (que idealmente son muy pocos) es atrapar la excepción que causó el problema y evaluar un determinado código para seguir adelante de forma correcta. Para eso primero tenemos que saber qué parte del código a ejecutar es el que podría terminar en excepción, luego qué tipo de error queremos tratar y finalmente qué se debería hacer al respecto. El siguiente código va a usar el vola: de Golondrina que puede romperse:

`[ pepita vola: 100 ] on: Error do: [:error | Transcript show: error messageText ]`

Mostrar en el Transcript (un workspace especial que pueden encontrar en Tools -&gt; Transcript) no es un manejo razonable de problemas, es sólo a modo de ejemplo. Algo lógico sería que el usuario pueda ver una ventanita indicando el problema para hacer algo al respecto.

Un problema que tiene la línea anterior es que para cualquier problema se va a mostrar el texto en el Transcript, si pepita apuntara a nil y el error es que nil no entiende vola: también resolvería el problema con el bloque que escribe en el Transcript en vez de abrir el debugger :O Y si quiero hacer cosas distintas ante problemas distintos?

Acá entra en juego lo de mandarle signal: a las clases de la jerarquía. Yo puedo tener clases propias que hereden de Error que sean particulares del dominio en el que estoy trabajando. Eso permite atrapar sólo las excepciones que me interesan y dejar pasar las que no sé cómo manejar para que alguien más se ocupe. Por ejemplo:

`[ pepita vola: 100 ] on: NoSePuedeVolarError do: [:error | Transcript show: error messageText ]`

Estrategias para manejar errores
--------------------------------

-   La forma por excelencia de lidiar con un error es **no hacer nada!!**. La mayoría de las veces no tenemos la capacidad de recuperarnos del problema en el mismo lugar donde se produce, lo más sano es dejarla burbujear hasta el punto en donde sí haya algo para hacer al respecto.
-   Atraparla, hacer algo y continuar con el flujo normal de ejecución
-   Atraparla, hacer algo y volver a lanzar la misma excepción. Eso se puede hacer mandando signal al error atrapado en el bloque que maneja el problema.
-   Atraparla y lanzar otra más adecuada agregando más información del problema.

Pueden leer un poquito más sobre este tema [acá](manejo-de-errores.html)
