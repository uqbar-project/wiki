---
layout: article
title: Aplicacion parcial
---

Por aplicación parcial se entiende a la [aplicación de una función](aplicacion.html), pero suministrando menos parámetros que los que esta requiere. **El resultado de aplicar parcialmente una función es otra función que espera menos parámetros que la original**, ya que puede realizar reemplazos en su definición por expresiones o valores concretos. La aplicación parcial es muy útil para [componer funciones](composicion.html) y para parametrizar funciones de [Orden Superior](orden-superior.html).

Por ejemplo, las siguientes expresiones presentan aplicación parcial:

-   `take 3`
-   `(+1)`
-   `max "hola"`

Para visualizar mejor la transformación que ocurre al aplicar parcialmente una función pueden consultar el tipo de las funciones sin aplicar y parcialmente aplicadas usando :t en el editor de Haskell. Podemos realizar un análisis en función del tipo de las expresiones anteriores, cuantos más parámetros se aplican menor aridad (cantidad de parámetros) tiene la función resultante:

```Haskell
*Main> :t take
take :: Int -> [a] -> [a]
*Main> :t take 3
take 3 :: [a] -> [a]

*Main> :t (+)
(+) :: Num a => a -> a -> a
*Main> :t (+1)
(+1) :: Num a => a -> a

*Main> :t max
max :: Ord a => a -> a -> a
*Main> :t max "hola"
max "hola" :: [Char] -> [Char]
```

En el caso de max "hola" vemos también que no sólo disminuyó la cantidad de parámetros, sino que el tipo de la función resultante es más particular, ya que "hola" obliga a que el otro valor esperado por el max también sea de tipo String para poder compararlos y determinar cuál es mayor en orden alfabético.

Las siguientes funciones no están aplicadas parcialmente:

-   `take` (no está aplicada)
-   `odd` (no está aplicada)
-   `odd 3` (está completamente aplicada, esta expresión no es de tipo función sino que es un booleano)
-   `max 0 3` (está completamente aplicada, esta expresión no es de tipo función sino que es un número)

Una consecuencia de esto es que sólo pueden aplicarse parcialmente funciones de 2 o más argumentos. Para que la aplicación parcial exista, es necesario que las funciones estén currificadas (ver [Currificación](currificacion.html)).

Puedo aplicar parcialmente el segundo parámetro en vez del primero?
-------------------------------------------------------------------

En ocasiones sucede que no podemos aplicar parcialmente una función ya que el valor que le queremos pasar no es el primero que espera sino otro, por ejemplo si quiero saber si un nombre es exótico, que se cumple si tiene x, k, q o w, no sería correcto intentar hacer:

`esExotico nombre = any (elem "XKQWxkqw") nombre`

Ya que "xkqw" que es la lista en la cual quiero verificar si se encuentra uno de los caracteres del nombre, no es correcto tratar de aplicárselo a elem porque debería ser el segundo parámetro, no el primero. De hecho esa función va a compilar correctamente, pero no va a funcionar como esperamos, ya que al intentar usarla de esta forma:

`> esExotico "Xiomara"`

Nosotros esperaríamos que nos diga True, pero vamos a tener un error de tipos:

``    Couldn't match expected type `[ [Char] ]' with actual type `Char' ``

Esto sucede porque si a elem le aplicamos un String (equivalente a \[Char\]), el resultado va a ser una función de tipo \[ \[Char\] \] -> Bool

Formas posibles de resolverlo:

**Usando una [expresión lambda](expresiones-lambda.html)**

`esExotico nombre = any (\letra -> elem letra "XKQWxkqw") nombre`

**Usando notación infija (como los operadores) en vez de prefija**: En ocasiones nos parece más natural usar las funciones de dos argumentos de forma infija, por ejemplo:

`` > 10 `mod` 2 ``

Podemos aprovechar ese feature para aplicar el segundo parámetro y no el primero como hacemos con los operadores, por ej. `(/2)`

`` esExotico nombre = any (`elem` "XKQWxkqw") nombre ``

**Usando la función flip**

En Haskell existe una función de [Orden Superior](orden-superior.html) llamada flip cuyo tipo es `(a -> b -> c) -> b -> a -> c`, y sirve justamente para resolver esta clase de problemas ya que lo que hace es aplicar la función que recibe con los parámetros en el orden inverso al que le llegan. Podríamos usar flip parcialmente aplicada para lograr nuestro objetivo.

`esExotico nombre = any (flip elem "XKQWxkqw") nombre`
