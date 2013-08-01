Variables mágicas
-----------------

Algo que se ve mucho en los parciales es gente que inventa variables mágicas por no entender orden superior y aplicación parcial, por ejemplo hacer algo como:

`aprobados alumnos = filter (nota `**`alumno`**` > 4) alumnos`

en vez de:

`aprobados alumnos = filter ((>4).nota) alumnos`

o en todo caso usando [Expresiones lambda](expresiones-lambda.html):

`aprobados alumnos = filter (\alumno -> nota alumno > 4) alumnos`

La variable alumno de la solución errónea no existe, y si existiera estaríamos cayendo en el problema explicado en la siguiente sección.

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

Moraleja:

> ***No es lo mismo** un booleano/entero/string/etc **que una función que devuelve** un booleano/entero/string/etc*

Composición y aplicación parcial
--------------------------------

Muchas veces se ven errores que (a veces disfrazados de un me confundí con los paréntesis) llevan a expresiones inválidas, lo importante es entender qué construcciones son válidas y cuáles no.

Normalmente hay muchas formas de llegar al mismo resultado. Por ejemplo:

`(not.esDivisor año) 100`
`not (esDivisor año 100)`

Estas dos lineas producen el mismo resultado. Mientras que las siguientes están mal:

`not.esDivisor año 100`
`(not.esDivisor) año 100`
`(otroBooleano && (not.esDivisor año)) 100`

Porqué? Repasemos las que estaban bien:

**`(not.esDivisor` `año)`**` 100`

Lo que está resaltado es una **función**. Es resultado de componer otras dos funciones: **not** y **esDivisor año**. Notá como me refiero a **esDivisor año** como una única función, que NO es **esDivisor**. Para ver que son diferentes, podemos solo chequear sus tipos:

`'''esDivisor::Int->Int->Bool`
`esDivisor año::Int->Bool'''`

Esta es una versión simplificada del tipo, no es exactamente lo que diría haskell, pero a efectos de la explicación da igual. Notá que **esDivisor año** espera un argumento menos que **esDivisor** (justamente, porque ya le pasaste el año). Obviamente, si aplicamos esta función otra vez, esto se repite.

**`esDivisor` `año` `100` `::` `Bool`**

Esto ya no es una función. Es un booleano. Ya no puedo aplicarlo.

Habiendo entendido esto, miremos otra vez la composición:

`(not.esDivisor año)`

Si digo que se está componiendo **not** con **esDivisor año**, podés deducir que la aplicación de una función tiene "mayor precedencia" que la composición. Una forma de entender esto es que la aplicación tiene prioridad, entonces la composición va a trabajar con el resultado de la aplicación.

Comparemos esto con uno de los casos de error:

`(not.esDivisor año) 100      `**`<-` `Funciona` `bien.` `Le` `aplico` `100` `al` `resultado` `de` `la` `composición`**
`not.esDivisor año 100        `**`<-` `Falla!` `Trata` `de` `componer` `not` `con` `esDivisor` `año` `100,` `que` `es` `un` `Bool,` `no` `una` `función.`**

Los otros casos erroneos también podemos deducirlos así:

`(not.esDivisor) año 100      `**`<-` `Falla` `porque` `tratás` `de` `componer` `esDivisor,` `que` `espera` `más` `de` `un` `argumento.`**
`(otroBooleano && (not.esDivisor año)) 100   `**`<-` `Falla` `porque` `(not.esDivisor` `año)` `es` `una` `función` `y` `&&` `espera` `un` `booleano.`**
