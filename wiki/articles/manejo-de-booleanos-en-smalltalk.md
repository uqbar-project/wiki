Los Booleanos como objetos
--------------------------

Los booleanos son objetos que representan la condición de verdad ó falsedad.

Recordemos que `even` es un mensaje que entienden los números, y devuelve un booleano. (true si el número es par)

`6 even. "devuelve true"`
`true not. "devuelve false"`
`false not. "devuelve true"`

Aquí se ve que **`not`** es un mensaje que entienden los booleanos (y que devuelve el booleano opuesto).

Hay otros mensajes que devuelven booleanos:

`5 < 1 "devuelve false"`
`1 between: 0 and: 3 "devuelve true"`

Entonces, si queremos que un método devuelva un booleano, basta con devolver el mismo resultado que obtuvimos. Por ejemplo,

`"la golondrina pepita está empachada si su energía es mayor que 100"`
`>>estasEmpachada`
`    ^ energia > 100.`

Conjunción y Disjunción Lógica
------------------------------

`true | false "devuelve true"`
`true & false "devuelve false"`

Para saber si el 6 es par y divisible por 3:

`6 even & (6 isDivisibleBy: 3) "devuelve true"`

### Agregando Vitaminas

Además de los mensajes binarios `&` y `|`, tenemos los mensajes `and:` y `or:`, que en vez de recibir un booleano, reciben un [bloque de código](bloques.html) (que adentro tiene un booleano). Veamos:

`6 even | 5 even "devuelve true"`
`6 even or: [5 even]. "devuelve true"`

`5 even & 6 even. "devuelve false"`
`5 even and: [6 even]. "devuelve false"`

¿Para qué queremos estos mensajes? Porque son "lazy". Es decir, tienen algo de [evaluación perezosa](estrategias-de-evaluacion-visi-c3-b3n-operativa.html). Ésto significa que si estoy haciendo un "or" y el receptor ya es verdadero, no hace falta analizar el segundo elemento (el or es verdadero):

`5 > 1 or: [7 * 2]. "devuelve true, sin analizar el bloque basura"`
`5 > 1 | (7 * 2). "tira error, porque 14 no es un booleano"`

Y si estoy haciendo un "and" y el receptor es falso, no hace falta analizar el segundo elemento (el and es falso):

`5 even and: [4]. "devuelve false, sin analizar el bloque basura"`
`5 even & 4. "tira error, porque 4 no es un booleano"`

En general, es buena práctica utilizar and: y or:.

### Para pensar

¿Qué pasaría en cada caso si dentro del bloque hay una operación con [efecto](transparencia-referencial--efecto-de-lado-y-asignaci-c3-b3n-destructiva.html) sobre el estado del sistema? La respuesta a ésta pregunta nos va a ayudar a entender por qué no está bueno tener efecto cuando hacemos un and: ó un or:.

Errores Comunes
---------------

### true vs True

En el ambiente hay un objeto llamado **true** y otro llamado **True**.

El que representa el valor de verdad, el "sí", es el **true** con minúscula.

True es la clase de la cual true es instancia. Lo que empieza en mayúscula es casi-casi siempre nombre de clase (o de variable de clase, eso lo vemos cerca del final de la parte de objetos).

Si uno se confunde y usa True, claro, el ifTrue: no anda bien, porque el mensaje ifTrue: lo entiende el objeto true, no su clase.

### ifTrue: \[^true\], ¿está bien?

<img src="RobinMalUsoBooleanos.jpg" title="fig:Robin siendo abofeteado por Batman. Debería hacer mejor uso de booleanos." alt="Robin siendo abofeteado por Batman. Debería hacer mejor uso de booleanos." width="300" /> Miremos este método que está en una clase cuyas instancias entienden el mensaje pais

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

=== ^(algo = true) === Una variante del caso anterior es un código como este, dentro de una clase cuyas instancias entienden los mensajes estaLibre y estaAndando

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

#### Un caso parecido

¿Y si en lugar de estaLibre tengo estaOcupado, qué hago, pongo

`   puedeUsarse`
`       ^(self estaOcupado = false) & (self estaAndando = true)`

nooooo ... quiero el booleano "contrario" al resultado de `self` `estaLibre`, para eso los booleanos entienden not, en este caso

`   puedeUsarse`
`       ^(self estaOcupado not) & (self estaAndando)`
