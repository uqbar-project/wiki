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

En ningún caso vamos a poder modificar desde fuera del objeto que tiene una referencia el valor de la misma, siempre hay que mandarle un mensaje a ese objeto para que la cambie. Esto está relacionado con la idea de [encapsulamiento](encapsulamiento.html), que es una de las bases del paradigma de objetos.

Referenciando nuevas instancias (Smalltalk nativo)
--------------------------------------------------

En las líneas del estilo

`  unaVar := UnaClase new.`

pasan **dos** cosas, en el orden que se indica

1.  se crea un objeto instancia de la clase UnaClase.
2.  se hace que la variable unaVar haga referencia al objeto recién creado.

### Un ejemplito

Miremos esta secuencia de sentencias, se numeran para hablar de ellas abajo.

`  1. col1 := OrderedCollection new.`
`  2. col1 add: Jugador new.`
`  3. col1 add: (Carta palo: 'espada' numero: 3).`
`  4. col1 add: 'hola'.`
`  5. jug := col1 first.`
`  6. tacar := col1 second.`
`  7. elPalo := tacar palo.`
`  8. jug agregarCarta: (Carta palo: 'oro' numero: 4).`
`  9. col2 := col1.`
` 10. col2 add: 38.`

Comentarios

-   en la línea 1 se crea una instancia de OrderedCollection, y además la variable col1 es asignada con esa instancia nueva.
-   en las líneas 2 y 3 se crean dos objetos (un jugador y una carta) sin que se asignen a ninguna variable (no nos importa cómo maneja su estado la colección).
-   en las líneas 5, 6 y 7 se asignan variables con referencias a objetos que no se crean en la misma línea, que "vienen desde antes".
-   en la línea 8, otra vez se crea un objeto sin asignarlo a una variable.
-   en la línea 9, las variable col2 pasa a apuntar al **mismo** objeto que es apuntado por la variable col1
-   por lo tanto si después de la línea 10 pido `col1` `size` me va a decir 4.

