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

Conviene prestar atención a lo siguiente: lo que devuelve `filter` `even`, coincide con lo que recibe `head`. Eso hace posible la composición.

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

Errores comunes
---------------

Supongamos una lista de alumnos representados con tuplas de tipo (nombre::String, nota::Int). Queremos obtener los nombres de los alumnos aprobados (nota &gt;= 4).

Podemos suponer además la existencia de las funciones:

`nombre (nom, _) = nom`
`aprobado (_, not) = not >= 4`

Un error que veo con frecuencia es hacer:

`aprobados alumnos = map nombre . filter aprobado alumnos`

La composición es una operación *entre funciones* esto quiere decir que a ambos lados del "." **debe** haber una función. ¿Qué hay a cada lado del "." en este caso:

-   , no hay problema: es una función (de listas de alumnos/tuplas en listas de nombres);

-   ... sí hay problema! No es una función, es una lista de alumnos.

Diciéndolo "en fácil": le tiene que faltar un parámetro, si yo a una función le aplico todos los parámetros deja de ser una función y pasa a ser un valor "simple". Tal vez sea interesante pensar que por efecto de la [currificación](currificacion.html), en Haskell los parámetros se pueden pasar a las funciones de a uno, entonces:

-   es una función de dos parámetros.

-   es una función de un parámetro (tenía dos, le apliqué uno, queda uno más por aplicar).

-   es una lista de alumnos, no es más una función, no se le pueden aplicar parámetros.

Sólo puede ser usado en una composición (para ser estricto la [composición de funciones de dos o más parámetros](composicion-de-funciones-de-dos-o-mas-parametros.html) es posible, pero excede a lo que hacemos en la materia así que a los efectos de PDEP podemos olvidarnos de esa posibilidad).

### Formas correctas de hacerlo:

Sin composición  

`aprobados alumnos = map nombre (filter aprobado alumnos)`

  
Es decir uso *aplicación* en lugar de composición (ojo, esto funciona pero si en estamos en un parcial y se desea evaluar que el alumno sepa composición... ahí no están usando composición entonces puede no ser suficiente como solución al ejercicio).

Con composición  

`aprobados = map nombre . filter aprobado`

  
Claramente son funciones las dos expresiones a ambos lados del ".". (Notese que a la derecha del "=" también hay un parámetro menos.)

### Composición vs. Aplicación

Para terminar de entenderlo recuerden la matemática, ¿es lo mismo que ? Claramente si g es una función yo no puedo hacer . Por otro lado, si en lugar de una función g tuviera un valor real x, entonces puedo hacer pero no .

### Algunos detalles técnicos

Si bien en general intentamos concentrarnos en los conceptos y no prestar tanta atención al conocimiento en sí del lenguaje; para poder expresar correctamente una composición en Haskell es necesario comprender correctamente algunos detalles de la sintaxis del Haskell:

1.  Si pongo un "." es composición, si no es aplicación.
      
    Los dos conceptos son bien distintos y es muy importante comprender la diferencia; por lo tanto es necesario ser bien explícito sobre cuándo se esta queriendo utilizar uno u otro. En criollo, se tiene que notar dónde hay un punto y dónde no.

2.  El operador de composición tiene mayor precedencia que todos los demás
      
    Por lo tanto la expresión debe leerse como .

    De la misma manera si yo pongo se lee como y es incorrecto por lo expresado en el punto anterior.

    Una alternativa posible es alterar la precedencia explícitamente usando paréntesis, por ejemplo ; donde "primero" se componen las funciones, eso produce una nueva función y a esa nueva función le aplico la lista como parámetro.

    Para más información puede leer: [Precedencia de los operadores más comunes en Haskell](precedencia-de-los-operadores-mas-comunes-en-haskell.html) y [Cuándo usar paréntesis](cuando-usar-parentesis.html)


