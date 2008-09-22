true vs True
------------

En el ambiente hay un objeto llamado true y otro llamado True.

El que representa el valor de verdad, el "sí", es el true con minúscula.

True es la clase de la cual true es instancia. Lo que empieza en mayúscula es casi-casi siempre nombre de clase (o de variable de clase, eso lo vemos cerca del final de la parte de objetos).

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
