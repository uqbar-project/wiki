Por aplicación parcial se entiende a la [aplicación de una función](aplicacion.html), pero suministrando menos parámetros que los que esta requiere. **El resultado de aplicar parcialmente una función es otra función que espera menos parámetros que la original**, ya que puede realizar reemplazos en su definición por expresiones o valores concretos. La aplicación parcial es muy útil para [componer funciones](composicion.html) y para parametrizar funciones de [Orden Superior](orden-superior.html).

Por ejemplo, las siguientes expresiones presentan aplicación parcial:

-   `map` `fst`
-   `(+1)`
-   `foldl` `(+)`
-   `foldl` `(+)` `0`

Para visualizar mejor la transformación que ocurre al aplicar parcialmente una función pueden consultar el tipo de la función map y de la función map fst usando :t en el editor de Haskell. Podemos realizar un análisis en función del tipo de las expresiones anteriores, cuantos más parámetros se aplican menor aridad (cantidad de parámetros) tiene la función resultante:

`*Main> :t map`
`map :: (a -> b) -> [a] -> [b]`
`*Main> :t map fst`
`map fst :: [(b, b1)] -> [b]`

`*Main> :t (+)`
`(+) :: Num a => a -> a -> a`
`*Main> :t (+1)`
`(+1) :: Num a => a -> a`

`*Main> :t foldl`
`foldl :: (a -> b -> a) -> a -> [b] -> a`
`*Main> :t foldl (+)`
`foldl (+) :: Num b => b -> [b] -> b`
`*Main> :t foldl (+) 0`
`foldl (+) 0 :: Num b => [b] -> b`

En el caso de map fst y del fold (+) vemos también que no sólo disminuyó la cantidad de parámetros, sino que el tipo de la función resultante es más particular, ya que fst obliga a que la lista sea una lista de duplas y el + a que la lista y el valor inicial del foldl sean números.

Las siguientes funciones no están aplicadas parcialmente:

-   `all` (no está aplicada)
-   `odd` (no está aplicada)
-   `odd` `3` (está completamente aplicada, esta expresión no es de tipo función sino que es un booleano)
-   `filter` `(>5)` `[4,5,9,10]` (está completamente aplicada, esta expresión no es de tipo función sino que es una lista de números)

Una consecuencia de esto es que sólo pueden aplicarse parcialmente funciones de 2 o más argumentos. Para que la aplicación parcial exista, es necesario que las funciones estén currificadas (ver [Currificación](currificacion.html)).
