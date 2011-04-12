Para salir del paso...
----------------------

Al programar en `Haskell` suele pasar que *muy* frecuentemente tenemos problemas con los tipos numéricos, en particular suele ocurrir al intentar hacer divisiones. Por ejemplo en el siguiente programa, la función `division`

`division xs = sum xs / length xs`

Produce el siguiente error:

`ERROR - Unresolved overloading`
`*** Type       : Fractional Int => [Int] -> Int`
`*** Expression : \xs -> sum xs / lgth xs`

El problema surge porque el `Haskell` tiene diferentes tipos de valores numéricos (enteros, reales, etc) un sistema de tipos muy estricto que hace que no sea sencillo mezclar los distintos tipos de valores en una misma operación.

La versión corta es que `length` `xs` es un entero y la operación `/` no está definida para los números enteros. Por lo tanto es necesario convertir el resultado de `length` `xs` al tipo de datos adecuado, utilizando la función `fromIntegral`:

`division xs = sum xs / length xs`

Esto funciona siempre que yo quiera hacer una división no-entera y uno de los parámetros (o ambos) es un entero. Para hacer divisiones enteras tenemos las funciones `div` y `mod`.

`division xs = sum xs / fromIntegral (length xs)`

Ó bien:

`division xs = fromIntegral (sum xs) / fromIntegral (length xs)`

La historia completa
--------------------

### ¿La división no está definida para los enteros?

Para comprender la totalidad del problema, es necesario comprender el tipo de la función `(/)`:

`Main> :t (/)`
`(/) :: Fractional a => a -> a -> a`

Se puede ver que `(/)` es una función polimórfica, es decir que puede ser utilizada con diferentes tipos de datos. Sin embargo no puede ser utilizada con *cualquier* tipo de dato; la restricción es que el tipo tiene que ser *instancia* de la clase `Fractional`. En particular, los tipos enteros de Haskell (`Int` e `Integer`) no son instancias de `Fractional`, por lo tanto siempre que se tenga un valor de alguno de esos tipos deberá ser *convertido* utilizando la función `fromIntegral`.

Con esa idea en mente, analizamos los tipos de las funciones del ejemplo de la sección anterior:

`Main> :t sum`
`sum :: Num a => [a] -> a`
`Main> :t length`
`length :: [b] -> Int`

Y podemos concluir que sum no tendrá problemas, porque funciona para cualquier tipo numérico (por definición todas las instancias de `Fractional` son instancias de `Num`.

En cambio la función `length` no es polimórfica en cuanto a su valor de retorno; aunque puede recibir cualquier tipo de lista, siempre devuelve un `Int`. Entonces el resultado de `length` no puede ser parámetro de `(/)`.

### Algunos casos más complicados

Supongamos que no utilizamos la función `sum`, y en cambio queremos definir la nuestra propia, de la siguiente manera:

`suma = foldr (+) 0`

O bien (para no definir una que ya existe), podemos definir

`prod = foldr (*) 1`

¿Funcionan las siguientes definiciones?

`div2  xs = suma xs / fromIntegral (length xs)`
`floca xs = prod xs / fromIntegral (length xs)`

Si intentamos hacer eso, obtendremos el siguiente error:

`ERROR `[`file:.\pruebas.hs:15`](file:.\pruebas.hs:15)` - Instance of Fractional Integer required for definition of div2`

Para entender por qué se produce esto, debemos analizar nuevamente los tipos de `suma` y `prod`:

`Hugs> :t suma`
`suma :: [Integer] -> Integer`
`Main> :t prod`
`prod :: [Integer] -> Integer`

Y como se puede ver devuelven valores de tipo `Integer` que, como dijimos antes, no es instancia de `Fractional` y por lo tanto no funciona.

La idea de la sección anterior también puede servir en este caso (aunque no es la única forma):

`div2  xs = fromIntegral (suma xs) / fromIntegral (length xs)`
`floca xs = fromIntegral (prod xs) / fromIntegral (length xs)`

### Aprovechando mejor el polimorfismo

En realidad hay una forma mejor de solucionar el problema planteado en la sección. La desventaja de la solución descripta consiste en que, al resolverlo de esa manera, la función suma sólo funciona para valores de tipo `Integer`; lo que puede resultar muy restrictivo en un futuro.

¿Por qué sucede esto? Como sabemos, el `Haskell` trabaja con [inferencia de tipos](inferencia-de-tipos.html), por lo tanto, cuando nosotros no indicamos en nuestro programa de qué tipo son los valores que espera y devuelve una función, el sistema intentará descubrir ese tipo por nosotros. Normalmente este mecanismo de inferencia es suficiente y resulta de gran utilidad, ya que nos permite tener un lenguaje fuertemente tipado que nos ayuda a encontrar muchos errores en nuestro código en una fase previa a la ejecución del programa, al mismo tiempo que nos evita de la burocracia y el engorro de tener que indicar el tipo de cada función explícitamente.

Sin embargo en este caso el tipo inferido por el sistema de tipos de `Haskell` es *subóptimo*. El tipo inferido es `[Integer]` `->` `Integer`, es decir, que sólo funciona para listas de tipo `Integer`.

Sin embargo, si miramos la definición de `suma`, podemos ver que se basa en las funciones `foldr` y `(+)`, cuyos tipos son:

`Main> :t foldr`
`foldr :: (a -> b -> b) -> b -> [a] -> b`
`Main> :t (+)`
`(+) :: Num a => a -> a -> a`

Es decir que no hay ningún motivo para restringir el tipo a `[Integer]` `->` `Integer`, dado que `foldr` funciona para cualquier tipo de lista y `(+)` funciona para todos los tipos de la familia `Num` (es decir, todos los tipos numéricos predefinidos del Haskell). De hecho, si consultamos el tipo de la expresión `foldr` `(+)` `0`, obtenemos:

`Main> :t foldr (+) 0`
`foldr (+) 0 :: Num a => [a] -> a`

Que es el tipo que buscamos.

Lamentablemente, cuando intenta inferir un tipo automáticament para la función `suma`, el `Haskell` le asigna un tipo *monomórfico*, defaulteando en el tipo `Integer`. La forma de evitar esta restricción es haciendo explícita nuestra intención de que la función suma sea *polmórfica*, esto se logra asociando a la función una indicación del tipo esperado.

El tipo esperado no es otro que el de la expresión `foldr` `(+)` `0`, es decir, `Num` `a` `=>` `[a]` `->` `a`. La definición de la función quedaría así:

`suma :: Num a => [a] -> a`
`suma = foldr (+) 0`

Al hacer ese cambio la siguiente definición pasa a estar correctamente tipada:

`div2  xs = suma xs / fromIntegral (length xs)`
