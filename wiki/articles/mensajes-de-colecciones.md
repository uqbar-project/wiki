Este es un pequeño resumen de mensajes que entiende una colección. Lo que sigue sirve para todos los [sabores de colecciones](sabores-de-colecciones.html).

¿Qué puedo hacer con los conjuntos de objetos? Muchas cosas, Smalltalk tiene mensajes de colecciones muy útiles para ayudarnos a abstraernos un poquito. Vamos a separarlos en los que tienen [efecto colateral](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html) sobre la colección receptora y los que no.

Con efecto
----------

Estos mensajes alteran a la colección que recibe el mensaje. Si no quiero perder la colección original puedo o bien copiar la colección mandándole shallowCopy y luego usar estos mensajes, o replantearme si no quería usar alguno de los mensajes de colecciones sin efecto :)

### Agregarle objetos

¿Que mensaje envío? add:

`pajaros add: pepita. `
`usuarios add: usuario23. `

También existe el mensaje addAll: que recibe otra colección y agrega todos los elementos del parámetro a la colección receptora.

`pajaros addAll: picaflores`

### Quitarle objetos

¿Que mensaje envío? remove:

`pajaros remove: pepita. `
`usuarios remove: usuario23. `

Análogamente al addAll: tenemos el removeAll: que le quita a la receptora todos los que estén en la colección parametrizada.

`pajaros removeAll: picaflores`

Sin efecto
----------

Estos mensajes se pueden mandar tantas veces como queramos sin miedo de alterar la colección receptora.

### Saber cuántos objetos tiene

¿Que mensaje envío? size

¿Que me devuelve? Un objeto número con la cantidad de objetos que conoce.

`pajaros size.`

### Preguntarle si tiene un objeto

¿Que mensaje envío? includes:

¿Que me devuelve? Un objeto booleano, true o false

`pajaros includes: pepita. `
`usuarios includes: usuario23. `

### Unirlo con otro conjunto

¿Que mensaje envío? union:

¿Que me devuelve? Una nueva colección con la unión de ambas.

`golondrinas union: picaflores.`
`mujeres union: hombres.`

### Seleccionar los elementos que cumplen con un criterio

¿Que mensaje envío? select:

¿Que me devuelve? Una nueva colección con los objetos que cuando se los evalúa con el bloque, dan true.

`pajaros select: [:unPajaro | unPajaro estaDebil ].`
`usuarios select: [:unUsuario | unUsuario deuda > 1000].`

### Seleccionar los elementos que no cumplen con un criterio

¿Que mensaje envío? reject:

¿Que me devuelve? Una nueva colección con los objetos que cuando se los evalúa con el bloque, dan false.

`pajaros reject: [:unPajaro | (unPajaro estaDebil | unPajaro estaExcitado)].`
`usuarios reject: [:unUsuario | unUsuario esDeudor].`

### Buscar algún elementos que cumpla con un criterio

¿Que mensaje envío? detect:

¿Que me devuelve? Un objeto de la colección que cuando se lo evalúe con el bloque, de true.

`pajaros detect: [:unPajaro | unPajaro estaDebil ].`
`usuarios detect: [:unUsuario | unUsuario deuda > 1000].`

¿Qué pasa si no hay ningún objeto que cumpla la condición? Explota, lo cual tiene sentido porque no sabe qué hacer si no lo tiene. Por eso existe otra versión en la cual podemos decirle qué devolvernos si no hay ninguno.

`pajaros detect: [:unPajaro | unPajaro estaDebil ] ifNone: [ pajaros anyOne ].`

### Recolectar el resultado de hacer algo con cada elemento

¿Que mensaje envío? collect:

¿Que me devuelve? Una nueva colección con los objetos que devuelve el bloque.

`pajaros collect: [:unPajaro | unPajaro ultimoLugarDondeFue].`
`usuarios collect: [:unUsuario | unUsuario nombre].`

### Verificar si todos los elementos de la colección cumplen con un criterio

¿Que mensaje envío? allSatisfy:

¿Que me devuelve? True, si todos los objetos de la colección dan true al evaluarlos con el bloque.

`pajaros allSatisfy: [:unPajaro | unPajaro estaDebil].`
`usuarios allSatisfy: [:unUsuario | unUsuario gastaMucho].`

### Verificar si algún elemento de la colección cumple con un criterio

¿Que mensaje envío? anySatisfy:

¿Que me devuelve? True, si alguno de los objetos de la colección da true al evaluarlo con el bloque.

`pajaros anySatisfy: [:unPajaro | unPajaro estaDebil].`
`usuarios anySatisfy: [:unUsuario | unUsuario gastaMucho].`

### Reducir una colección haciendo algo un elemento y el resultado de la reducción con el elemento anterior

Si queremos evaluar un bloque de dos parámetros con cada elemento de la colección, usando como primer parámetro la evaluación previa, y como segundo parámetro ese elemento.

¿Que mensaje envío? inject:into:

¿Que me devuelve? La ultima evaluación del bloque.

¿Para qué me sirve? Para muchas cosas: obtener el que maximice o minimice algo, obtener el resultado de una operación sobre todos (p.ej. sumatoria), y más.

Un ejemplo (sumatoria):

`pajaros inject: 0 into: [:inicial :unPajaro | inicial + unPajaro peso]. `

Otro ejemplo (recolecto colección de colecciones):

`#( 1 2 3 ) inject: Set new into: [:divisores :nro | divisores union: (nro divisores)] `

Y otro más (el que tiene más energía):

`pajaros inject: (pajaros anyOne) into: [:masFuerte :unPajaro | (unPajaro energia < masFuerte energia) ifTrue: [unPajaro] ifFalse: [masFuerte]]`

### Obtener una colección de otro sabor

Existen los mensajes asSet, asBag, asOrderedCollection y asSortedCollection: que retornan una colección nueva a partir de la receptora que tiene otras características (ver [sabores de colecciones](sabores-de-colecciones.html)). En particular asSet y asSortedCollection: son de uso más común para quitar repetidos u ordenar en base a un criterio respectivamente.

Y el do:?
---------

El mensaje do: sirve para hacer algo con cada objeto de la colección. El do: en sí mismo no tiene efecto colateral, pero tampoco tiene un valor de retorno que pueda interesarnos. Entonces, cuándo se usa? Sólo tiene sentido usar do: cuando lo que yo quiero hacer sobre cada elemento (o sea, el bloque) sí tiene un efecto.

`pajaros do: [:unPajaro | unPajaro vola: 100 ].`

Yo no pretendo recolectar resultados, sólo quiero que pasen cosas con cada elemento.

**Importante:** no usar el do: para todo, los mensajes que vimos recién como el select:, el collect:, el anySatisfy:... son abtracciones mucho más ricas que nos permiten concentrarnos en nuestro objetivo y no en el algoritmo para lograr el resultado, seamos declarativos!
