Funciones de orden superior
---------------------------

Es importante diferenciar:

-   Qué valores son funciones y qué valores no.
-   En qué lugares se espera poner una función y en qué lugares se espera otro tipo de valor.

Por ejemplo

`sonTodasPasables pelicula = all (puntajePromedio pelicula > 6) (snd pelicula)`

tiene un error porque la función espera como parámetro una función, mientras que no lo es (denota un valor booleano).
