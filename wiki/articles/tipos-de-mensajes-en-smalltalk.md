Primero arranquemos por lo básico. Todo envío de mensaje sigue la siguiente regla: OBJETO MENSAJE PARAMETRO Lo primero que aparece es el objeto receptor, luego viene el mensaje que se le envía a dicho objeto el cual puede o no tener parámetros.

Smalltalk es bastante particular en aspectos sintácticos, por eso es importante detenerse a entender cómo se interpreta. Hay 3 tipos de mensajes en Smalltalk.

Los **mensajes unarios** son aquellos que no reciben parámetros, o sea que el receptor solito puede resolver lo pedido. Un ejemplo de esto sería la negación de un booleano:

` true not`
`   --> retorna false`

También existen los **mensajes binarios** que son los que reciben sólo un parámetro y su nombre está compuesto por símbolos (no alfanumérico), por ejemplo la suma entre dos números es un mensaje que recibe un parámetro solo (el número a sumar), el otro número es el receptor del mensaje.

` 2 + 5`
`   --> retorna 7`

Por último están los **mensajes de palabra clave** que pueden recibir tantos parámetros como sean necesarios. Estos mensajes se caracterizan por tener una o más partes alfanuméricas terminadas por el caracter : luego de los cuales se pasa cada parámetro (o sea, los parámetros van intercalados) como se muestra en los siguientes ejemplos:

` 5 raisedTo: 2`
`   --> es un mensaje que recibe el objeto 5 con un único parámetro, el 2, y retorna 25`

` 3 between: 10 and: 25`
`   --> es un mensaje que recibe el objeto 3 con un 2 parámetros, el 10 es el primer parámetro y el 25 es el segundo, y retorna false`

Lo que tiene de simpático los parámetros intercalados es la expresividad, pero hay que ser cuidadosos de no cometer errores. Por ejemplo si quisiéramos saber si el 3 está entre 10 y el 5 elevado al cuadrado debemos escribirlo de esta forma:

` 3 between: 10 and: (5 raisedTo: 2)`
`   --> el segundo parámetro del mensaje between:and: es el resultado de mandarle raisedTo: a 5 con el parámetro 2, lógicamente también retorna false`

Esos paréntesis son inportantes para que se interprete como queremos y no como un único envío de mensajes, donde el mensaje sería between:and:raisedTo: de 3 parámetros, que los números no entienden!

La [precedencia de mensajes](precedencia-de-mensajes.html) en Smalltalk se basa en estos 3 tipos de mensajes, sólo nos va a interesar esta diferenciación por ese motivo.
