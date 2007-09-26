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
