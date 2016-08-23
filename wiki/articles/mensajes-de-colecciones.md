Este es un pequeño resumen de mensajes que entiende una colección. Lo que sigue sirve para todos los [sabores de colecciones](sabores-de-colecciones.html).

¿Qué puedo hacer con los conjuntos de objetos? Muchas cosas, las colecciones entienden mensajes muy útiles para ayudarnos a abstraernos un poquito. Vamos a separarlos en los que tienen [efecto colateral](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html) sobre la colección receptora y los que no.

Con efecto
----------

Estos mensajes alteran a la colección que recibe el mensaje. Si no quiero perder la colección original puedo o bien copiar la colección mandándole shallowCopy y luego usar estos mensajes, o replantearme si no quería usar alguno de los mensajes de colecciones sin efecto :)

### Agregarle objetos

**`Smalltalk`**
`pajaros add: pepita. `
`usuarios add: usuario23. `

**`Wollok`**
`pajaros.add(pepita)`
`usuarios.add(usuario23)`

También existe el mensaje addAll: en Smalltalk y addAll(conjunto) en Wollok que recibe otra colección y agrega todos los elementos del parámetro a la colección receptora.

**`Smalltalk`**
`pajaros addAll: picaflores`

**`Wollok`**
`pajaros.addAll(picaflores)`

### Quitarle objetos

**`Smalltalk`**
`pajaros remove: pepita. `
`usuarios remove: usuario23. `

**`Wollok`**
`pajaros.remove(pepita)`
`usuarios.remove(usuario23) `

Análogamente al addAll:/addAll(conjunto), existe el removeAll:/removeAll(conjunto) que le quita a la receptora todos los que estén en la colección parametrizada.

Sin efecto
----------

Estos mensajes se pueden mandar tantas veces como queramos sin miedo de alterar la colección receptora.

### Saber cuántos objetos tiene

**`Smalltalk`**
`pajaros size.`

**`Wollok`**
`pajaros.size()`

¿Que me devuelve? Un objeto número con la cantidad de objetos que conoce.

### Preguntarle si tiene un objeto

**`Smalltalk`**
`pajaros includes: pepita. `
`usuarios includes: usuario23. `

**`Wollok`**
`pajaros.contains(pepita)`
`usuarios.contains(usuario23)`

¿Que me devuelve? Un objeto booleano, true o false

### Unirlo o intersectarlo con otro conjunto

**`Smalltalk`**
`golondrinas union: picaflores.`
`mujeres union: hombres.`

**`Wollok`**
`golondrinas.union(picaflores)`
`mujeres.union(hombres)`

¿Que me devuelve? Una nueva colección con la unión de ambas.

**`Smalltalk`**
`aves intersection: voladores.`

**`Wollok`**
`aves.intersection(voladores)`

¿Que me devuelve? Una nueva colección con la intersección de ambas.

Siendo que la unión y la intersección está pensada para conjuntos matemáticos, estos mensajes sólo lo entienden los sets.

Además en Wollok puede hacerse:

`golondrinas + picaflores`

Que retorna la concatenación de ambos conjuntos, retornando una colección del mismo sabor que la receptora.

### Seleccionar los elementos que cumplen con un criterio

**`Smalltalk`**
`pajaros select: [:unPajaro | unPajaro estaDebil ].`
`usuarios select: [:unUsuario | unUsuario deuda > 1000].`

**`Wollok`**
`pajaros.filter({unPajaro => unPajaro.estaDebil()})`
`usuarios.filter({unUsuario => unUsuario.deuda() > 1000})`

¿Que me devuelve? Una nueva colección con los objetos que cuando se los evalúa con el bloque, dan true.

### Buscar algún elementos que cumpla con un criterio

**`Smalltalk`**
`pajaros detect: [:unPajaro | unPajaro estaDebil ].`
`usuarios detect: [:unUsuario | unUsuario deuda > 1000].`

**`Wollok`**
`pajaros.find({unPajaro => unPajaro.estaDebil()})`
`usuarios.find({unUsuario => unUsuario.deuda() > 1000})`

¿Que me devuelve? Un objeto de la colección que cuando se lo evalúe con el bloque, de true.

¿Qué pasa si no hay ningún objeto que cumpla la condición? Explota, lo cual tiene sentido porque no sabe qué hacer si no lo tiene. Por eso existe otra versión en la cual podemos decirle qué devolvernos si no hay ninguno.

**`Smalltalk`**
`pajaros detect: [:unPajaro | unPajaro estaDebil ] ifNone: [ pajaros anyOne ].`

**`Wollok`**
`pajaros.findOrElse({unPajaro => unPajaro.estaDebil()}, { pajaros.anyOne() })`

### Recolectar el resultado de hacer algo con cada elemento

**`Smalltalk`**
`pajaros collect: [:unPajaro | unPajaro ultimoLugarDondeFue].`
`usuarios collect: [:unUsuario | unUsuario nombre].`

**`Wollok`**
`pajaros.map({unPajaro => unPajaro.ultimoLugarDondeFue()}`
`usuarios.map({unUsuario => unUsuario.nombre()}`

¿Que me devuelve? Una nueva colección con los objetos que devuelve el bloque.

### Verificar si todos los elementos de la colección cumplen con un criterio

**`Smalltalk`**
`pajaros allSatisfy: [:unPajaro | unPajaro estaDebil].`
`usuarios allSatisfy: [:unUsuario | unUsuario gastaMucho].`

**`Wollok`**
`pajaros.all({unPajaro => unPajaro.estaDebil()})`
`usuarios.all({unUsuario=> unUsuario.gastaMucho()})`

¿Que me devuelve? un booleano que indique si todos los objetos de la colección dan true al evaluarlos con el bloque.

### Verificar si algún elemento de la colección cumple con un criterio

**`Smalltalk`**
`pajaros anySatisfy: [:unPajaro | unPajaro estaDebil].`
`usuarios anySatisfy: [:unUsuario | unUsuario gastaMucho].`

**`Wollok`**
`pajaros.any({unPajaro => unPajaro.estaDebil()})`
`usuarios.any({unUsuario=> unUsuario.gastaMucho()})`

¿Que me devuelve? un booleano que indique si alguno de los objetos de la colección da true al evaluarlo con el bloque.

### Reducir una colección haciendo algo un elemento y el resultado de la reducción con el elemento anterior

Si queremos evaluar un bloque de dos parámetros con cada elemento de la colección, usando como primer parámetro la evaluación previa, y como segundo parámetro ese elemento (o sea, lo que en funcional vimos como [fold](fold.html))

**`Smalltalk`**
`pajaros inject: 0 into: [:inicial :unPajaro | inicial + unPajaro peso].  "sumatoria de pesos"`
`pajaros inject: (pajaros anyOne) into: [:masFuerte :unPajaro | `
`  (unPajaro energia < masFuerte energia) ifTrue: [unPajaro] ifFalse: [masFuerte]] "maximo segun energia"`

**`Wollok`**
`pajaros.fold(0, {inicial, unPajaro => inicial + unPajaro.peso()}) // sumatoria de pesos`
`pajaros.fold(pajaros.anyOne(), {masFuerte, unPajaro => `
`  if(unPajaro.energia() < masFuerte.energia()) unPajaro else masFuerte }) // maximo segun energia`

¿Que me devuelve? La ultima evaluación del bloque.

¿Para qué me sirve? Para muchas cosas: obtener el que maximice o minimice algo, obtener el resultado de una operación sobre todos (p.ej. sumatoria), y más. Por lo general existen operaciones de más alto nivel para la mayoría de las reducciones más comunes, de ser así obviamente es preferible usar esos mensajes en vez de hacer la reducción.

Por ejemplo, en Wollok podríamos haber hecho directamente:

`pajaros.sum({unPajaro => unPajaro.peso()})`
`pajaros.max({unPajaro => unPajaro.energia()})`

### Obtener una colección de otro sabor

En **Smalltalk** existen los mensajes asSet, asBag, asOrderedCollection y asSortedCollection: que retornan una colección nueva a partir de la receptora que tiene otras características (ver [sabores de colecciones](sabores-de-colecciones.html)). En particular asSet y asSortedCollection: son de uso más común para quitar repetidos u ordenar en base a un criterio respectivamente.

`usuarios.asSortedCollection: [:unUsuairo :otroUsuario | unUsuairo edad < otroUsuario edad ]`

En **Wollok**, al sólo haber sets y listas, lo que ambas entienden son asSet() y asList(). Las listas ordenadas por un criterio pueden obtenerse mediante el mensaje sortedBy(criterio), que retorna una nueva colección con los elementos de la receptora ordenadas según el bloque recibido.

`usuarios.sortedBy({ unUsuairo, otroUsuario => unUsuairo.edad() < otroUsuario.edad() })`

¿Y la iteración?
----------------

El mensaje do: en Smalltalk, forEach(bloque) en Wollok, sirve para hacer algo con cada objeto de la colección. Este mensaje en sí mismo no tiene efecto colateral, pero tampoco tiene un valor de retorno que pueda interesarnos. Entonces, ¿cuándo se usa? Sólo tiene sentido usar este mensaje cuando lo que queremos hacer sobre cada elemento (o sea, el bloque) sí produce un efecto.

**`Smalltalk`**
`pajaros do: [:unPajaro | unPajaro vola: 100 ].`

**`Wollok`**
`pajaros.forEach({unPajaro => unPajaro.vola(100)})`

Yo no pretendo recolectar resultados, sólo quiero que pasen cosas con cada elemento.

**Importante:** no usar el do:/forEach para todo, los mensajes que vimos anteriormente son abtracciones mucho más ricas que nos permiten concentrarnos en nuestro objetivo y no en el algoritmo para lograr el resultado, **seamos declarativos**!
