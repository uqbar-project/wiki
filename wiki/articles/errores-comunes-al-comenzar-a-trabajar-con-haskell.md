Funciones y valores
-------------------

Es importante diferenciar:

-   Qué valores son funciones y qué valores no.
-   En qué lugares se espera poner una función y en qué lugares se espera otro tipo de valor.

Por ejemplo

`sonTodasPasables pelicula = all (puntajePromedio pelicula > 6) (snd pelicula)`

tiene un error porque la función espera como parámetro una función, mientras que **no es una función** (denota un valor booleano).

\_ Otro ejemplo:

`puntajes pelicula = snd pelicula`
`esRiesgosa = elem 1 puntajes`

¿Cuál es el problema? La función espera como segundo parámetro **una lista**, sin embargo la expresión no es una lista, sino *una función que devuelve una lista*.

> ***No es lo mismo un booleano/entero/string/etc que una función que devuelve un booleano/entero/string/etc***
