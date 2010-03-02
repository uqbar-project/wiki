Tal vez convenga aclarar que esta no es una categorización teórica, es más bien una introducción y vamos de lo más simple a lo más complejo.

Tipos simples y compuestos
--------------------------

### Básicos: caracteres y booleanos

Los tipos más básicos que tenemos en Haskell son los booleanos y los caracteres, que tiene los tipos y respectivamente:

`Prelude> :t True`
`True :: Bool`
`Prelude> :t 'a'`
`'a' :: Char`

Los números son un poquito más complejos porque tenemos números enteros, reales, racionales y muchas más variantes así que por un momento los vamos a dejar afuera.

### Listas y Strings

Un String es simplemente una lista de caracteres; se puede escribir o :

`Prelude> :t "Hola"`
`"Hola" :: [Char]`

Más aún, para cualquier lista el tipo se escribe poniendo entre corchetes el tipo de los elementos la lista, por ejemplo una lista de booleanos:

`Prelude> :t [True, False]`
`[True, False] :: [Bool]`

También una lista de Strings (o lista de listas de caracteres):

`Prelude> :t ["Hola", "Chau"]`
`["Hola", "Chau"] :: [ [Char] ]`

### Tuplas

Una tupla es también un valor compuesto. A diferencia de las listas el número de componentes es fijo y los pueden ser cada uno de un tipo distinto. (Esto está más detallado en [¿Cuál es la diferencia entre una tupla y una lista?](-cual-es-la-diferencia-entre-una-tupla-y-una-lista-.html), pero antes te recomiendo mirar los ejemplos que siguen.)

Una de las tuplas más simples que se puede imaginar es:

`Prelude> :t (True, 'H')`
`(True, 'H') :: `**`(Bool,` `Char)`**

es decir, una tupla compuesta por un booleano y un caracter. Sin embargo, los elementos de las tuplas también pueden ser compuestos, como un String:

`Prelude> :t (True, "Hola")`
`(True, "Hola") :: (Bool, `**`[Char]`**`)`

o inclusive el componente de una tupla puede ser otra tupla:

`Prelude> :t (False, ('H', "ola"))`
`(False, ('H', "ola")) :: `**`(Bool,` `(Char,` `[Char]))`**` `

También podemos tener tuplas de más de dos componentes

`Prelude> :t (True, 'H', [False])`
`(True, 'H', [False]) :: (Bool, Char, [Bool])`

es decir, un booleano, un caracter y una lista de booleanos.

Para finalizar podemos ver un ejemplo en el que combianmos todo lo anterior (no es trivial, tomate tu tiempo para leerlo!):

`Prelude> :t ([True, False, True], "Chau", [(True, "True"), (False, "False")])`
`([True, False, True], "Chau", [(True, "True"), (False, "False")])`
`  :: ([Bool], [Char], [(Bool, [Char])])`

Es decir, una tupla de tres componentes, a saber:

-   Una lista de booleanos
-   Un string o lista de caracteres
-   Una lista de tuplas cuyo primer componente es un booleano y su segundo componente es un string.

Funciones
---------

### Funciones con un único parámetro

El tipo de una función que tiene un parámetro se indica relacionando mediante el símbolo la entrada o dominio de la función con la salida o imagen. Por ejemplo la función not recibe un booleano y devuelve otro:

`Prelude> :t not`
`not :: Bool -> Bool`

La función recibe un caracter y devuelve un booleano:

`Prelude> :t Char.isLower`
`Char.isLower :: Char -> Bool`

Y la función recibe una lista de booleanos y devuelve un booleano (*and*ea todos los booleanos de la lista)

`Prelude> :t and`
`and :: [Bool] -> Bool`

### Funciones con más de un parámetro

Las funciones de más de un parámetro tienen alguna sutileza porque en Haskell se trabaja con el concepto de [Currificación](currificacion.html), entonces una función que nosotros en matemática estaríamos acostumbrados a verla como en Haskell la vamos a escribir . Las funciones de dos parámetros cuyo tipo tiene esa forma se denominan *currificadas*.

(A los efectos de entender el sistema de tipos podemos pensarlo simplemente como una función que recibe dos booleanos, aunque en realidad la versión currificada es mucho más poderosa. Para más detalles ver la teoría sobre [Currificación](currificacion.html).)

El tipo que usamos como ejemplo en el párrafo anterior corresponde (entre otros) a la función

`Prelude> :t (&&)`
`(&&) :: Bool -> Bool -> Bool`

Aplicación
----------

La aplicación es uno de los temas que tal vez más confunden cuando se habla de tipos de datos. La confusión más frecuente radica en no diferenciar correctamente *una expresión que tiene valor Booleano* de *una función que devuelve Booleanos*.

Ya vimos dos ejemplos de funciones que devuelven booleanos, con uno y dos parámetros:

`Prelude> :t not`
`not :: Bool -> Bool`
`Prelude> :t Char.isLower`
`Char.isLower :: Char -> Bool`
`Prelude> :t and`
`and :: [Bool] -> Bool`
`Prelude> :t (&&)`
`(&&) :: Bool -> Bool -> Bool`

En este punto es importante entender que ninguno de estos ejemplos es un *valor booleano*. Cuando veo el tipo eso se entiende como el tipo *de las funciones a las que si les aplico un parámetro de tipo Char producen un valor de tipo Bool*, que claramente no es lo mismo que el tipo Bool.

Lo dicho, si le aplicamos los parámetros adecuados a esas funciones, podemos obtener valores booleanos:

`*Main> :t not True`
`not True :: Bool`
`*Main> :t Char.isLower 'a'`
`Char.isLower 'a' :: Bool`
`*Main> :t and [True, False, True]`
`and [True, False, True] :: Bool`
`*Main> :t True && False`
`True && False :: Bool`

En síntesis es un valor booleano, en cambio no es un valor booleano, es una función que devuelve booleanos. Ambas cosas no se pueden mezclar.

### Aplicacion Parcial

Polimorfismo
------------

TBC: head, fst, length
