Por aplicación parcial se entiende a la [aplicación de una función](aplicacion.html), pero suministrando menos parámetros que los que esta requiere. El resultado de aplicar parcialmente una función es otra función que espera menos parámetros que la original, ya que puede realizar reemplazos en su definición por expresiones o valores concretos. La aplicación parcial es muy útil para [componer funciones](composicion.html) y para parametrizar funciones de [Orden Superior](orden-superior.html).

Por ejemplo, las siguientes expresiones presentan aplicación parcial:

-   `map` `fst`
-   `(+1)`
-   `foldl` `(+)`
-   `foldl` `(+)` `0`

Para visualizar mejor la transformación que ocurre al aplicar parcialmente una función pueden consultar el tipo de la función map y de la función map fst usando :t en el editor de Haskell.

Mientras que las siguientes, no:

-   `all` (no está aplicada)
-   `odd` (no está aplicada)
-   `filter` `(>5)` `[4,5,9,10]` (está completamente aplicada)

Una consecuencia de esto es que solo pueden aplicarse parcialmente funciones de 2 o más argumentos. Para que la aplicación parcial exista, es necesario que las funciones estén currificadas (ver [Currificación](currificacion.html)).
