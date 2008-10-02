true vs True
------------

En el ambiente hay un objeto llamado **true** y otro llamado **True**.

El que representa el valor de verdad, el "sí", es el **true** con minúscula.

True es la clase de la cual true es instancia. Lo que empieza en mayúscula es casi-casi siempre nombre de clase (o de variable de clase, eso lo vemos cerca del final de la parte de objetos).

Si uno se confunde y usa True, claro, el ifTrue: no anda bien, porque el mensaje ifTrue: lo entiende el objeto true, no su clase.

ifTrue: \[^true\], ¿está bien?
------------------------------

Miremos este método que está en una clase cuyas instancias entienden el mensaje pais

`   mismoPaisCon: unaCiudad `
`       ((self pais) = (unaCiudad pais))`
`       ifTrue: [^true]`
`       ifFalse: [^false]`

y supongamos que la comparación

`   ((self pais) = (unaCiudad pais))`

da true.

¿Qué quiere decir que "da true"? Que si evalúo esa parte, el objeto que me devuelve es el objeto true, el **único** objeto true de Smalltalk. A ese le estoy diciendo ifTrue:ifFalse:, y en el bloque que ejecuta si es true ... le digo que devuelva true, o sea que devuelva el mismo objeto al que le dije ifTrue:ifFalse:. Lo mismo con el false.

En resumen, el ifTrue:ifFalse: sobra, este método se puede escribir así

`   mismoPaisCon: unaCiudad `
`       ^(self pais) = (unaCiudad pais)`

y listo.

== ^(algo = true) == Una variante del caso anterior es un código como este, dentro de una clase cuyas instancias entienden los mensajes estaLibre y estaAndando

`   puedeUsarse`
`       ^(self estaLibre = true) & (self estaAndando = true)`

Analicemos la parte

`   (self estaLibre = true)`

La expresión `self` `estaLibre` hace lo único que puede hacer una expresión en Smalltalk: devolver un objeto. ¿Qué objeto puede ser ese?

**1.**
Si la condición es cierta, entonces el objeto que devuelve es el objeto true, el único objeto en un ambiente Smalltalk que representa el valor de verdad "cierto". Si a ese objeto le pregunto `=` `true`, ¿qué objeto va a ser el resultado?

Si pregunto si dos objetos son iguales, o me va a responder true, o me va responder false, no hay otra. En este caso, true es el mismo objeto que true, o sea el = da cierto, o sea ... devuelve ¡true!, que es el mismo objeto que obtenía con

`   (self estaLibre)`

**2.**
Ahora supongamos que la condición no es cierta, en ese caso me devuelve el objeto false, si a false le digo `=` `true` el resultado de eso es el objeto false, que otra vez es lo mismo que obtengo poniendo solamente

`   (self estaLibre)`

**Consecuencia**
El mismo análisis lo puedo hacer con

`   (self estaAndando = true)`

y la conclusión es que el método puede escribirse así

`   puedeUsarse`
`       ^(self estaLibre) & (self estaAndando)`

En esta versión estamos manejando mejor los booleanos, porque aceptamos que el resultado de `self` `estaLibre` es un booleano, que va a ser true o false, y que va a entender `&` con el otro booleano como parámetro.

### Un caso parecido

¿Y si en lugar de estaLibre tengo estaOcupado, qué hago, pongo

`   puedeUsarse`
`       ^(self estaOcupado = false) & (self estaAndando = true)`

nooooo ... quiero el booleano "contrario" al resultado de `self` `estaLibre`, para eso los booleanos entienden not, en este caso

`   puedeUsarse`
`       ^(self estaOcupado not) & (self estaAndando)`
