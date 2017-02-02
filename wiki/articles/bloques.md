---
layout: article
title: Bloques
---

¿Qué es un bloque?
------------------

Un bloque es un objeto, y por lo tanto podemos hacer con él lo mismo que hacíamos con los demás objetos esto es:

-   Enviarle mensajes
-   Pasarlo como parámetro de algún mensaje
-   Hacer que una variable lo referencie

Un objeto bloque representa un cacho de código que no se ejecutó, ese código solo se ejecutará si alguien le manda un mensaje que le indique hacerlo.

Si ejecutamos las siguientes instrucciones:

**`Smalltalk`**
`x := 0.`
`[ x := x + 1].`

**`Wollok`**
`var x = 0`
`{ x = x + 1 }`

¿A qué objeto apunta la variable x después de ejecutar esas 2 líneas de código? Claro, al objeto cero (0)

Para que la variable x apunte al objeto uno (1) tenemos que decirle al bloque que se ejecute.

**`Smalltalk`**
`x := 0.`
`[ x := x + 1] value.`

**`Wollok`**
`var x = 0`
`{ x = x + 1 }.apply()`

Ahora sí, después de ejecutar esas 2 líneas de código la variable x va a apuntar al objeto uno (1). Es importante darse cuenta que value / apply() es un mensaje que le llega al objeto bloque. Este mensaje hace que se ejecute el código que está dentro del bloque y que se retorne el objeto devuelto por la última sentencia del bloque.

Bloques como funciones
----------------------

También se puede ver a los bloques como objetos que representan una función sin nombre (o sea, una función anónima, como las [Expresiones lambda](expresiones-lambda.html) de funcional!).

El bloque `[` `1` `]` en Smalltalk o `{` `1` `}` en Wollok es una función constante que siempre devuelve 1 si le decís que se ejecute. Pero el chiste de las funciones es que reciban parámetros y los bloques también pueden recibir parámetros, por ejemplo la sintaxis para un bloque de dos parámetros es:

**`Smalltalk`**
`[ :parametro1 :parametro2 | cuerpoDelBloque ]`

**`Wollok`**
`{ parametro1, parametro2 => cuerpoDelBloque }`

Ejemplos:

Podríamos representar las siguientes funciones...

`f(x) = 2x`
`g(x,y) = x2 + y2`

... con bloques de la siguiente forma:

**`Smalltalk`**
`f := [ :x | 2 * x ]`
`g := [ :x :y | (x raisedTo: 2) + (y raisedTo: 2) ]`

**`Wollok`**
`var f = {x => 2 * x }`
`var g = {x, y => x ** 2 + y ** 2 }`

¿Cómo hacemos para usar bloques que tienen parámetros? Por ejemplo si quisiéramos los equivalentes a evaluar las funciones f(4) y g(4,3)

**`Smalltalk`**
`f value: 4 "Esto devuelve el objeto 8"`
`g value: 4 value: 3 "Esto devuelve el objeto 25"`

**`Wollok`**
`f.apply(4) // Esto devuelve el objeto 8`
`g.apply(4, 3) // Esto devuelve el objeto 25`

Por lo general el uso que le damos a los bloques es sólo la creación de los mismos para pasar por parámetro a funcionalidad ya existente de propósito general, como son los mensajes de colecciones [Mensajes de colecciones](mensajes-de-colecciones.html) y el if de Smalltalk, no tanto la aplicación de los mismos.

¿Cómo funciona el \#ifTrue: y el \#ifFalse: de Smalltalk?
---------------------------------------------------------

En Smalltalk no existe la estructura de control if/else, sino que existen ditintos mensajes que entienden los booleanos para Si en un workspace escribimos

`pepita energia > 0 ifTrue: [ pepita come: 30 ]`

Pensando en términos de objeto y mensaje (mensaje = selector + parámetros) qué está pasando acá???

El objeto receptor del mensaje ifTrue: es el objeto que me devuelve pepita energia &gt; 0, ese objeto puede ser true o false.

Si el receptor es true queremos que el bloque \[ pepita come: 30 \] se ejecute. Si el receptor es false NO queremos que el bloque \[ pepita come: 30 \] se ejecute.

Entonces el objeto que tiene la responsabilidad de saber si el bloque debe o no ejecutarse es el booleano receptor del mensaje ifTrue:.

Siendo false la única instancia de la clase False y true la única instancia de la clase True, la implementación del método ifTrue: en cada una de las clases es

`True >> ifTrue: unBloque`
`  "self apunta a true entonces queremos que se ejecute el bloque"`
`  ^unBloque value `

`False >> ifTrue: unBloque`
`  "self apunta a false entonces NO queremos que se ejecute el bloque"`
`  ^nil`

O sea que las estructuras de control a las que estábamos acostumbrados por el paradigma estructurado, no son más que mensajes polimórficos :D Todo sigue las mismas reglas, objetos y mensajes
