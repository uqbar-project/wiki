*Ver también [Orden Superior](orden-superior.html)*

`Una consecuencia de implementar el pasaje de funciones como argumentos en lenguajes orientados a objetos# es que el envío de mensajes desde un objeto hacia otro ocurre en dos momentos: `

Un objeto A envía el mensaje de orden superior a otro B, pasando por argumento un bloque de código El objeto receptor B envía un mensaje al bloque de código, que es evaluado, potencialmente, enviando mensajes a objetos que sólo A conocía.

Es decir, mientras que en la programación imperativa el código cliente llama es aquel que llama a la biblioteca, aquí es la biblioteca quien termina llamando al código cliente, lo que se conoce como Inversión de Control (IoC).

object PruebaIoC {

`     def sumaDeInversas(valores: List[Int]) = valores.map { 1 / _ }.sum`
`}`

scala&gt; PruebaIoC.sumaDeInversas( List(4,0,6) )

java.lang.ArithmeticException: / by zero

`   at PruebaIoC$$anonfun$sumaDeInversas$1.apply$mcII$sp(:8)`
`   at PruebaIoC$$anonfun$sumaDeInversas$1.apply(:8)`
`   at PruebaIoC$$anonfun$sumaDeInversas$1.apply(:8)`
`   at scala.collection.TraversableLike$$anonfun$map$1.apply(TraversableLike.scala:194)`
`   at scala.collection.TraversableLike$$anonfun$map$1.apply(TraversableLike.scala:194)`
`   at scala.collection.LinearSeqOptimized$class.foreach(LinearSeqOptimized.scala:59)`
`   at scala.collection.immutable.List.foreach(List.scala:45)`
`   at scala.collection.TraversableLike$class.map(TraversableLike.scala:194)`
`   at scala.collection.immutable.List.map(List.scala:45)`
`   at PruebaIoC$.sumaDeInversas(:8)`

…..

-   Se pierde la secuencialidad
-   El control queda en manos de la biblioteca, que se constituye en un motor responsable de evaluar el código -&gt; ganamos en declaratividad
-   el codigo puede ser evaluado de forma diferente a la planteada por el cliente, incluso de forma diferida, asincrónica o ignorado completamente

evaluacion diferida mediante thunks
