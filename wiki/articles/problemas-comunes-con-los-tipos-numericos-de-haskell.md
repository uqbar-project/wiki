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

Si se da el caso que además se está utilizando una función propia (en lugar de una definidaCuando en lugar de utilizar alguna función que viene con el `Haskell` utilizamos alguna otra función definida por nosotros, es necesario realizar una verificación adicional. Esto es, comprobar que el tipo de la función definida se adecúa a las necesidades del programa.

Por ejemplo

Es un problema de tipos de datos (bastante frecuente). Lo que pasa es que length devuelve un entero y la división (/) no está definida para los enternos. Entonces tenés que convertir ese entero (Int) a real (Fractional), asi:

`   division (x:xs)= suma (x:xs) / fromInt (length (xs))`

Además deberías corregir el parámetro de length (¿por qué xs?) Deberías poner:

`   division (x:xs)= suma (x:xs) / fromInt (length (x:xs))`

Y de hecho no es necesario poner (x:xs) en todos lados, más simple sería:

`  division l = suma l / fromInt (length l)`

Notas
-----
