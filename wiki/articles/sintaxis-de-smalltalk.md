La sintaxis de Smalltalk es bastante particular, pero sigue unas pocas reglas generales fáciles de aprender:

La regla principal es que lo primero de la sentencia es un objeto seguido por el mensaje que le queremos mandar, el cual podría o no recibir parámetros. Por ejemplo:

`pepita energia`

El objeto receptor será aquel referenciado por la variable llamada pepita, al cual se le está mandando el mensaje energia que no recibe parámetros.

`4 + 5`

El objeto receptor es el número 4 y recibe el mensaje + con el parámetro 5.

`pepita vola: 10`

El objeto receptor será aquel referenciado por la variable llamada pepita, al cual se le está mandando el mensaje vola: con el parámetro 10. Más sobre este tema en [Tipos de mensajes en Smalltalk](tipos-de-mensajes-en-smalltalk.html).

Pero qué pasa con los elementos del lenguaje que no son envíos de mensajes?

Las sentencias se separan con un punto (es separador, no terminador, por eso no siempre hace falta escribirlo)

`pepita vola: 20.`
`pepita energia`

Este código primero hace que pepita vuele 20 y luego consulta su energia. En cambio esto...

`pepita vola: 20`
`pepita energia`

... va a tratar de mandarle el mensaje pepita al número 20 (lo cual claramente no es lo que queríamos) y como no va a entender ese mensaje, va a tirar un error.

Para asignar una variable se usa := que **no es un mensaje a un objeto**, en el método vola: de pepita (que tiene un atributo energia) podríamos tener lo siguiente:

`vola: metrosAVolar`
`  energia := energia - 40 - metrosAVolar`

Lo que va a suceder es que la referencia llamada energia va a modificarse, de modo que su nuevo valor será el resultado de la expresión energia - 40 - metrosAVolar. Sólo se puede tener una referencia a la izquierda del :=, es lo único que puede ser asignado.

Para retornar el valor de una expresión en un método se usa el circunflejo ^, podríamos definirle el siguiente método a pepita para retornar un booleano que diga si pepita tiene hambre

`tenesHambre`
`  ^ energia < 20`

El retorno corta la ejecución del método, motivo por el cual no tiene sentido tener otra sentencia luego de ^ energia &lt; 20, ya que nunca se va a evaluar.

En caso de que querramos definir variables locales para un método, se usan pipes | como se explica en [Variables locales en métodos](variables-locales-en-metodos.html)

Y los paréntesis se usan para delimitar partes de una expresión, por ejemplo para indicar dónde comienza y dónde termina un envío de mensajes para pasar por parámetro a otro mensaje el resultado de dicha evaluación. Sabiendo bien cómo trabaja la [precedencia](precedencia-de-mensajes.html) es posible ahorrarse muchos paréntesis innecesarios haciendo que sea más fácil de leer.

Importante! No confundir paréntesis ( ) con corchetes \[ \]. Los corchetes se usan para construir [bloques](bloques.html).
