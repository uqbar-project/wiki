Machaco un poquito más
----------------------

Cuando componés dos funciones, por ejemplo f y g, lo haces así

`f . g`

eso arma una nueeeva función, resultado de componer f con g.

¿Qué queremos decir con 'Componer'?  
Lo mismo que con composición de funciones en matemática.

fog(x) es lo mismo que f(g(x))

Es decir que lo que devuelve g, hay que aplicárselo a f. A tener en cuenta, tanto f como g tienen que ser funciones, no podemos componer una función con un 2, o un 2 con una lista.

> Sólo podemos componer funciones

### Ejemplo

`(2+).(3*)`

Esta formada por la composición de (2+) que recibe un numero y devuelve ese numero sumado con 2 y (3\*) que recibe un numero y devuelve ese numero multiplicado por 3. Esa composición te devuelve una nueva función que recibe un número y te devuelve el resultado de primero multiplicar ese numero por 3 y despues sumarle 2

`(2+) . (3*) 2       (espero se entiendan mis flechitas xD)`
`  ^  |   ^  |`
`  |__|   |__|`
` `

Fijate que el 2 se aplica a la función de la derecha y el resultado de eso se aplica a la funcion de la izquierda. Y ese sería el resultado final de la función.

### Un ejemplo un poco más complejo

`head . filter even`

Tanto lo de la izquierda como lo de la derecha del punto, tienen que ser funciones que reciban 1 parámetro (al menos en paradigmas, lo otro es más complejo). Y a su vez, el tipo que retorna la función de la derecha, tiene que ser el mismo que el tipo que espera recibir la función de la izquierda. En este caso, `filter` `even` es una función que recibe una lista de números y devuelve otra lista, con los pares de esa lista original.

`filter even :: Num a => [a] -> [a]`

y head recibe una lista y devuelve un elemento de esa lista, y por ende del mismo tipo que los elementos de la lista.

`head :: [a] -> a`

Fijate como lo que devuelve `filter` `even`, coincide con lo que recibe `head`. Eso hace posible la composición.

Preguntas Frecuentes
--------------------

### ¿Es necesesario que esten los puntos para que sea composicion?

<dl>
<dd>
Porque para mí está implícito que hay composición, si por ejemplo en lugar de:

`find f = head . filter f`

pongo

`find f = head (filter f)`

</dl>
**Sí, es necesario.** En el segundo ejemplo estas aplicando (filter f) a head, y head espera una lista, no una función y por ende, no anda:

`head:: [a] -> a`

En cambio, cuando hacés

`head . filter f`

estás armando una nueva función que recibe una lista, la filtra y después devuelve la cabeza. La forma de armar la función que querés según el primer ejemplo sería:

`head (filter even [1 .. 20])`

Y en el segundo se aplicaría así:

`(head . filter even) [1..20]`
