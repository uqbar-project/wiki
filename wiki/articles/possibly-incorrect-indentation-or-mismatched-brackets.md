El problema
===========

Un error muy común en Haskell, tanto para los que recién arrancan como para los más experimentados, es pifiarla con los enters, espacios o tabs. ¿Por qué? Haskell no usa llaves {} ni bloques do-end para delimitar secciones del código, sino que se basa en el uso de caracteres blancos (withespaces) para darse cuenta, por ejemplo, cuando termina una ecuación de una función.

Entonces, si ponemos tabs, espacios, enters de menos o de más, obtendremos un sólo error:

` parse error: “possibly incorrect indentation or mismatched brackets”`

Lo que significa: "che, revisá tus withespaces; mientras tanto no voy a intentar interpretar a tu código"

La solución
===========

La solución al problema depende de dónde hayas pifiado. Veamos algunos casos comunes:

Dejar espacios o tabs antes de la cabecera de la función
--------------------------------------------------------

La cabecera de la función debe arrancar justo "contra el margen".

Mal:

`   f x = x + 1`

Bien:

`f x = 1`

Poner enters de más entre dos ecuaciones de la misma función
------------------------------------------------------------

Las ecuaciones de una función deben estar siempre juntas, sin enters de más entre ellas

Mal:

`f 0 = 0`

`f x = x + 1`

Bien:

` f 0 = 0`
` f x = x + 1`
