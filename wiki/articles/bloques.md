¿Qué es un bloque?
------------------

Un bloque es un objeto, y por lo tanto podemos hacer con él lo mismo que hacíamos con los demás objetos esto es:

-   Enviarle mensajes
-   Pasarlo como parámetro de algún mensaje
-   Hacer que una variable lo referencie

Un objeto bloque representa un cacho de código que no se ejecutó, ese código solo se ejecutará si alguien le manda un mensaje que le indique hacerlo.

Si escribo en un workspace:

`x := 0.`
`[ x := x + 1].`

¿A qué objeto apunta la variable x después de ejecutar esas 2 líneas de código? Claro, al objeto cero (0)

Para que la variable x apunte al objeto uno (1) tenemos que decirle al bloque que se ejecute.

`x := 0.`
`[ x := x + 1] value.`

Ahora sí, después de ejecutar esas 2 líneas de código la variable x va a apuntar al objeto uno (1). Es importante darse cuenta que value es un mensaje que le llega al objeto bloque \[ x := x + 1\].

El mensaje value hace que se ejecute el código que está dentro del bloque y que se retorne el último objeto devuelto por la última sentencia del bloque.

Ejemplo:

`[ pepita come: 14. `
`  5 factorial. `
`  pepe sueldo.`
`  3 between: 1 and: 5. ] value. "Esto devuelve el objeto true porque es lo que devuelve el último envío de mensaje dentro del bloque"`

Bloques como funciones
----------------------

También se puede ver a los bloques como objetos que representan una función sin nombre (o sea, una función anónima).

El bloque \[ 1 \] es una función constante que siempre devuelve 1 si le decís que se ejecute.

\[ 1 \] value. "Esto devuelve el objeto uno (1)"

Pero el chiste de las funciones es que reciban parámetros y los bloques también pueden recibir parámetros

\[ :parametro1 :parametro2 :parametro3 ... :parametroN | cuerpoDelBloque \]

Ejemplos:

f(x) = 2x -&gt; \[ :x | 2 \* x \] value

g(x) = x2 -&gt; \[ :x | x raisedTo: 2 \]

h(x,y) = x2 + y2 -&gt; \[ :x :y | (x raisedTo: 2) + (y raisedTo: 2) \]

s(a,b,c) = cos a + sen b + c -&gt; \[ :a :b :c | a cos + b sin + c \] ¿Cómo le hacemos para usar bloques que tienen parámetros?

f(4) -&gt; \[ :x | 2 \* x \] value: 4 "Esto devuelve el objeto 8"

h(4,3) -&gt; \[ :x :y | (x raisedTo: 2) + (y raisedTo: 2) \] value: 4 value: 3 "Esto devuelve el objeto 25"

s(5,2,1) -&gt; \[ :a :b :c | a cos + b sin + c \] value: 5 value: 2 value: 1 "Esto devuelve el objeto 2.192959612288908"

Jugando con los bloques
-----------------------

bloque1 := \[ :unAve | unAve vola: 20. unAve come: 30 \]. bloque2 := \[ :unAve | unAve vola: 50. unAve come: 14 \].

bloque1 value: pepita. "Esto hace que pepita vuele 20 kilometros y morfe 30 gramos de alpiste" bloque2 value: pepita. "Esto hace que pepita vuele 50 kilometros y morfe 14 gramos de alpiste" bloque1 value: pepona. "Esto hace que pepona vuele 20 kilometros y morfe 30 gramos de alpiste"

¿Cómo funciona el \#ifTrue: y el \#ifFalse:?
--------------------------------------------

Si en un workspace escribimos

(pepita energia &gt; 0) ifTrue: \[ pepita come: 30 \]

Pensando en términos de objeto y mensaje (mensaje = selector + parámetros) qué está pasando acá???

El objeto receptor del mensaje ifTrue: es el objeto que me devuelve pepita energia &gt; 0, ese objeto puede ser true o false.

Si el receptor es true queremos que el bloque \[ pepita come: 30 \] se ejecute. Si el receptor es false NO queremos que el bloque \[ pepita come: 30 \] se ejecute.

Entonces el objeto que tiene la responsabilidad de saber si el bloque debe o no ejecutarse es el booleano receptor del mensaje ifTrue:.

Siendo false la única instancia de la clase False y true la única instancia de la clase True, la implementación del método ifTrue: en cada una de las clases es

True &gt;&gt; ifTrue: unBloque

`   "self apunta a true entonces queremos que se ejecute el bloque"`
`   ^unBloque value `

False &gt;&gt; ifTrue: unBloque

`   "self apunta a false entonces NO queremos que se ejecute el bloque"`
`   ^nil`
