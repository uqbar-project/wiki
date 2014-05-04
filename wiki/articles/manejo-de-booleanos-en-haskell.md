El Tipo Bool
------------

Los booleanos son datos que representan la condición de verdad o falsedad.

Recordemos que `even` es una función que recibe un número, y devuelve un booleano. (True si el número es par)

`> even 6 `
`True`
`> not True`
`False`
`> not False`
`True`

Aquí se ve que **`not`** es una función recibe un Bool (y que devuelve el booleano opuesto).

Hay otras funciones que devuelven booleanos:

`> 5 < 1 `
`True`
`> "mama" == "papa"`
`False`

Entonces, si queremos que una función devuelva un booleano, basta con devolver el mismo resultado que obtuvimos. Por ejemplo,

`"la golondrina pepita está empachada si su energía es mayor que 100"`
`estaEmpachada (_,energia) = energia > 100.`

Conjunción y Disjunción Lógica
------------------------------

`> True || False`
`True`
`> True && False`
`False`

Para saber si el 6 es par y divisible por 3:

`> even 6 && (esDivisible 6 3)`
`True`

Errores Comunes
---------------

### "true" vs true vs True

En Haskell,

-   true es un nombre de variable ó de función. Si trato de usarlo, me va a tirar el error "Not in Scope".

`> not true`
`` Not in scope: `true' ``

-   "true" es un String, por lo que si se lo mando a una función que espera un booleano, va a haber un error de tipos:

`> not "true"`
`` Couldn't match expected type `Bool' ``
`` with actual type `[Char]' ``

-   True es la manera correcta de referirse al booleano. True es un [Constructor](constructor.html) del tipo de dato Bool.

`> not True`
`False`

### Problemas con Booleanos y Guardas

-   Ver <http://uqbar-wiki.org/index.php?title=Funciones_por_Partes#Errores_Comunes>

=== (algo == True) === Una variante del caso anterior es un código como este, dentro de una clase cuyas instancias entienden los mensajes estaLibre y estaAndando

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
