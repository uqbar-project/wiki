Por aplicación parcial se entiende a la [aplicación de una función](aplicacion.html), pero suministrando menos parámetros que los que esta requiere. Por ejemplo, las siguientes expresiones presentan aplicación parcial:

-   `map` `fst`
-   `(+1)`
-   `foldl` `(+)`
-   `foldl` `(+)` `0`

Mientras que las siguientes, no:

-   `all` (no está aplicada)
-   `odd` (no está aplicada)
-   `filter` `(>5)` `[4,5,9,10]` (está completamente aplicada)

Una consecuencia de esto es que solo pueden aplicarse parcialmente funciones de 2 o más argumentos.

Para que la aplicación parcial exista, es necesario que las funciones estén currificadas (ver [Currificación](currificacion.html)).
