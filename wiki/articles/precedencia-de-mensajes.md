Muchas veces nos encontramos en la situación de querer mandar varios mensajes en una misma sentencia, donde hay varios objetos involucrados. ¿Qué mensaje se evalúa primero? ¿Cuál será el objeto que finalmente sea pasado como parámetro?

Ésto se ve con varios ejemplos:

`  2 + 3 squared.`

¿Cómo lo interpreto? ¿(2+3) al cuadrado? ¿2 + (3 al cuadrado)?

Otro ejemplo:

`  3 + 2 * 4`

¿Cómo lo interpreto? ¿(3+2) \* 4? ¿3 + (2\*4)?

Precedencia
-----------

Bueno, antes de saber cómo evaluarlo, hay que recordar que existen 3 [tipos de mensajes en Smalltalk](tipos-de-mensajes-en-smalltalk.html):

1.  Unarios: *pepita energía*
2.  Binarios: *3 + 2*
3.  De Palabra Clave: *pepita come: 20.*

Así, **de arriba para abajo**, es la *precedencia* de los mensajes. Más arriba en la lista está el mensaje, más *fuerza* tiene para "atraer" al objeto que tiene al lado.

¿Y qué sucede si hay dos mensajes con igual precedencia?

-   Se evalúa de izquierda a derecha.

El ejemplo final
----------------

La siguiente evaluación:

`  2 raisedTo: 5 - 1 sqrt.   `

Devuelve 16...

-   se evalúa 1 sqrt (da 1),
-   luego 5 - 1 (da 4),
-   luego 2 raisedTo: 4

Cortando la precedencia
-----------------------

¿Y si yo quiero que primero se haga la resta?

Uso paréntesis, como en cualquier otro lenguaje:

`  2 raisedTo: (5 - 1) sqrt.   `

Devuelve 4 (2 elevado a la 2)
