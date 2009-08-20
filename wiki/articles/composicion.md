Preguntas Frecuentes
--------------------

¿Es necesesario que esten los puntos para que sea composicion? Porque para mí está implícito que hay composición, si por ejemplo en lugar de:

`find f = head . filter f`

pongo

`find f = head (filter f)`

Sí, es necesario. En el segundo ejemplo estas aplicando (filter f) a head, y head espera una lista, no una función y por ende, no anda:

`head:: [a] -> a`

En cambio, cuando hacés

`head . filter f`

estás armando una nueva función que recibe una lista, la filtra y después devuelve la cabeza. La forma de armar la función que querés según el primer ejemplo sería:

`head (filter even [1 .. 20])`

Y en el segundo se aplicaría así: (head . filter even) \[1..20\]
