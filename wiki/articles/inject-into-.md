El **inject:into:** es un solo mensaje que entienden todas las colecciones, recibe 2 parámetros.

`  `

### Parámetros del **inject:into:**

El primero parámetro del **inject:into:** (valorInicialAcumulador) representa el objeto al cual va a apuntar el acumulador antes de empezar a evaluar el bloque (el 2do parámetro del **inject:into:**) sobre cada elemento de la colección.

El segundo parámetro del **inject:into:** (bloqueConDosParametros) es un bloque que va a ser evaluado sobre cada elemento de la colección y el acumulador actual.

bloqueConDosParametros = \[ :acumuladorActual :elementoDeLaColeccion | <acá escribimos el resultado que representa al nuevo valor del acumulador > \]

El **inject:into:** retorna el valor final del acumulador

### Sumatoria de una colección de números

Si col es una colección de números y queremos obtener su sumatoria podemos hacer

`  `

valorInicialAcumulador = 0

acumulador = sumatoria

elementoDeLaColeccion = numero

operación que retorna el nuevo valor del acumulador = sumatoria + numero

### Productoria de una colección de números

Si col es una colección de números y queremos obtener su productoria podemos hacer

`  `

valorInicialAcumulador = 1

acumulador = productoria

elementoDeLaColeccion = numero

operación que retorna el nuevo valor del acumulador = productoria \* numero

### Máximo de una colección de números

Si col es una colección de números y queremos obtener el máximo podemos hacer

`  `

valorInicialAcumulador = col anyOne (algún elemento de la colección)

acumulador = maximo

elementoDeLaColeccion = numero

operación que retorna el nuevo valor del acumulador = maximo max: numero

### La altura más alta de una colección de personas

Si col es una colección de personas y queremos obtener la altura más alta

`  `

valorInicialAcumulador = col anyOne altura (alguna altura de alguna persona de la colección)

acumulador = maximaAltura

elementoDeLaColeccion = persona

operación que retorna el nuevo valor del acumulador = maximaAltura max: persona altura

### La persona más alta de una colección de personas

Si col es una colección de personas y queremos obtener la persona más alta

`  `

valorInicialAcumulador = col anyOne (alguna persona de la colección)

acumulador = personaMasAlta

elementoDeLaColeccion = persona

operación que retorna el nuevo valor del acumulador = personaMasAlta altura &gt; persona altura ifTrue: \[persona\] ifFalse: \[personaMasAlta\]

Nota: si asumimos que persona es instancia de la clase Persona podemos hacer lo siguiente

`  `
