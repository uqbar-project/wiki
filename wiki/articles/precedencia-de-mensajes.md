---
layout: article
title: Precedencia de mensajes
---

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

`  2 raisedTo: 5 - 2 * 4 sqrt.   `

Devuelve 64...

-   se evalúa 4 sqrt (da 2),
-   luego 5 - 2 (da 3),
-   luego 3 \* 2 (da 6)
-   luego 2 raisedTo: 6

Sí, no hay precedencia de operadores como estamos acostumbrados, con lo cual el - tiene la misma precedencia que el \* ya que ambos son binarios.

¿Y si yo quiero que primero se haga la multiplicación?

Uso paréntesis, como en cualquier otro lenguaje:

`  2 raisedTo: 5 - (2 * 4 sqrt). `

Así se restaría el resultado de multiplicar 2 por la raíz de 4 al 5, con lo cual estaríamos elevando 2 a la 1.
