Muchachos, no olvidar: en objetos una variable es una **referencia** a un objeto.

En Smalltalk, cuando una variable no ha sido inicializada aún, la misma referencia al objeto `nil` que representa la nada misma (y entiende muy poquitos mensajes, porque no hay muchas cosas que uno quiera hacer con la nada).

En Wollok, la nada no está representada con un objeto, sino con un valor primitivo llamado `null`. Si en algún momento ven que al ejecutar su programa, el mismo arroja un error llamado NullPointerException, significa que trataron de mandar un mensaje a algo que esperaban que fuera un objeto, pero era null, con lo cual deberían revisar que las variables involucradas en dicha operación se encuentren inicializadas correctamente.

La [asignación](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html) de variables se logra de la siguiente forma:

En Smalltalk:

`  variable := expresion-que-devuelve-un-objeto`

En Wollok:

`  variable = expresion-que-devuelve-un-objeto`

y debe interpretarse que cuando se evalúa esta línea, la variable referencia al objeto resultado de la expresión de la derecha.

Entonces, al asignar una variable **no estoy creando ningún objeto** ni estoy cambiando al objeto referenciado anteriormente por dicha variable, sólo se cambia el objeto al que está apuntando esa referencia. O sea, si yo tengo esto:

En Smalltalk:

`#pepita  (que tiene un atributo energia definido)`
`>> energia: cantidad`
`    energia := cantidad`
`(y luego le mando)`
`pepita energia: 100.`
`pepita energia: 50.`

En Wollok:

`object pepita {`
`  var energia`
`  `
`  method energia(cantidad){`
`    energia = cantidad`
`  }`
`}`
`(y luego le mando)`
`pepita.energia(100)`
`pepita.energia(50)`

Al mandar el mensaje para settear la energía a pepita por primera vez (asumiendo que no había sido inicializada antes su energía), el atributo energia que tiene pepita pasa de apuntar a nil a apuntar a 100, y luego de mandar el segundo mensaje pasa de apuntar a 100 a apuntar a 50. O sea que sólo cambia a quién conoce pepita mediante la referencia energia.

SIEMPRE lo que se encuentre a la izquierda de la asignación debe ser una variable, no se puede asignar un objeto (y por el mismo motivo no se puede asignar un envío de mensajes). Las siguientes expresiones son inválidas:

` 3 := 5.   <--- 3 es un objeto, no una referencia!!!`
` pepita energia := 10.   <--- pepita energia es un envío de mensajes, no una referencia!!!`

Los mismos ejemplos en Wollok serían:

`3 = 5 <--- Ojo que no se está consultando por igualdad, si eso es lo que querés tenés que usar ==`
`pepita.energia() = 10`

En ningún caso vamos a poder modificar desde fuera del objeto que tiene una referencia el valor de la misma, siempre hay que mandarle un mensaje a ese objeto para que la cambie. Esto está relacionado con la idea de [encapsulamiento](encapsulamiento.html), que es una de las bases del paradigma de objetos.
