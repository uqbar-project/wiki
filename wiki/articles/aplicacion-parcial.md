Apicación Parcial
=================

Por aplicació parcial se entiende a la [aplicación de una función](aplicacion.html), pero suministrando menos parámetros que los que esta requiere. Por ejemplo, en las siguientes expresiones presentan aplicación parcial:

-   map fst
-   (+1)
-   foldl (+)
-   foldl (+) 0

Mientras que las siguientes, no:

Una consecuencia de esto es que solo pueden aplicarse funciones de 2 o más argumentos.

Para que la aplicación parcial exista, es necesario que las funciones estén currificadas.
