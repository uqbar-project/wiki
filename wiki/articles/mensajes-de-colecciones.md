Este es un pequeño resumen de mensajes que entiende una colección. Lo que sigue sirve para todos los [sabores de colecciones](sabores-de-colecciones.html).

¿Qué puedo hacer con los conjuntos de objetos?

Agregarle objetos
-----------------

¿Que mensaje envío? add:

`pajaros add: pepita. `
`usuarios add: usuario23. `

Quitarle objetos
----------------

¿Que mensaje envío? remove:

`pajaros remove: pepita. `
`usuarios remove: usuario23. `

Preguntarle si tiene un objeto
------------------------------

¿Que mensaje envío? includes: ¿Que me devuelve? Un objeto booleano, true o false

`pajaros includes: pepita. `
`usuarios includes: usuario23. `

Unirlo con otro conjunto
------------------------

¿Que mensaje envío? union: ¿Que me devuelve? Una nueva colección con la unión de ambas.

`golondrinas union: picaflores.`
`mujeres union: hombres.`

Seleccionar los elementos que cumplen con un criterio
-----------------------------------------------------

¿Que mensaje envío? select: ¿Que me devuelve? Una nueva colección con los objetos que cuando se los evalúa con el bloque, dan true.

`pajaros select: [:unPajaro | unPajaro estaDebil ].`
`usuarios select: [:unUsuario | unUsuario deuda > 1000].`

Seleccionar los elementos que no cumplen con un criterio
--------------------------------------------------------

¿Que mensaje envío? reject: ¿Que me devuelve? Una nueva colección con los objetos que cuando se los evalúa con el bloque, dan false.

`pajaros reject: [:unPajaro | (unPajaro estaDebil | unPajaro estaExcitado)].`
`usuarios reject: [:unUsuario | unUsuario esDeudor].`

Recolectar el resultado de hacer algo con cada elemento
-------------------------------------------------------

¿Que mensaje envío? collect: ¿Que me devuelve? Una nueva colección con los objetos que devuelve el bloque.

`pajaros collect: [:unPajaro | unPajaro ultimoLugarDondeFue].`
`usuarios collect: [:unUsuario | unUsuario nombre].`

Verificar si todos los elementos de la colección cumplen con un criterio
------------------------------------------------------------------------

¿Que mensaje envío? allSatisfy: ¿Que me devuelve? True, si todos los objetos de la colección dan true al evaluarlos con el bloque.

`pajaros allSatisfy: [:unPajaro | unPajaro estaDebil].`
`usuarios allSatisfy: [:unUsuario | unUsuario gastaMucho].`

Verificar si algún elemento de la colección cumple con un criterio
------------------------------------------------------------------

¿Que mensaje envío? anySatisfy: ¿Que me devuelve? True, si alguno de los objetos de la colección da true al evaluarlo con el bloque.

`pajaros anySatisfy: [:unPajaro | unPajaro estaDebil].`
`usuarios anySatisfy: [:unUsuario | unUsuario gastaMucho].`

Reducir una colección haciendo algo un elemento y el resultado de la reducción con el elemento anterior
-------------------------------------------------------------------------------------------------------

Si queremos evaluar un bloque de dos parámetros con cada elemento de la colección, usando como primer parámetro la evaluación previa, y como segundo parámetro ese elemento. ¿Que mensaje envío? inject:into: ¿Que me devuelve? La ultima evaluación del bloque. ¿Para qué me sirve? Para muchas cosas: obtener el que maximice o minimice algo, obtener el resultado de una operación sobre todos (p.ej. sumatoria), y más.

Un ejemplo (sumatoria):

`pajaros inject: 0 into: [:inicial :unPajaro | inicial + unPajaro peso]. `

Otro ejemplo (recolecto colección de colecciones):

`#( 1 2 3 ) inject: Set new into: [:divisores :nro | divisores union: (nro divisores)] `

Y otro más (el que tiene más energía):

`pajaros inject: (pajaros anyOne) into: [:masFuerte :unPajaro | (unPajaro energia < masFuerte energia) ifTrue: [unPajaro] ifFalse: [masFuerte]]`
