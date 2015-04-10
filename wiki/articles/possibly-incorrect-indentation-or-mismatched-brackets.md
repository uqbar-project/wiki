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

Poner enters de más en una expresión larga
------------------------------------------

A veces puede pasar que por un tema de legibilidad queremos partir escribir una sola expresion larga a lo largo de varias lineas (aunque en general, el problema ahí es que la expresión es larga por falta de [delegación](delegacion.html)). El tema es que tenemos que ser muy claros en indicarle a Haskell que la expresión no terminó.

Dos reglas:

-   Si hacemos eso, en general vamos a tener que poner la expresión entre paréntesis
-   Y todo caso, debemos respetar la identación

Mal:

`f x = max`
`          x`
`          1`

Mal:

`f x = (max`
`           x`
`1)`

Mal:

`f x = (max x `
`1)`

Bien (multiples líneas)

`f x = (max`
`           x`
`           1)`

Bien (una sola línea)

`f x = max x 1`
