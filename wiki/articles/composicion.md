Qué es composición?
-------------------

¿Qué queremos decir con 'Componer'?  
Lo mismo que con composición de funciones en matemática.

fog(x) es lo mismo que f(g(x))

Es decir que lo que devuelve g, hay que aplicárselo a f. A tener en cuenta, tanto f como g tienen que ser funciones, no podemos componer una función con un 2, o un 2 con una lista.

> Sólo podemos componer funciones

Para componer dos funciones en Haskell, por ejemplo f y g, se hace así

`f . g`

eso arma una nueeeva función, resultado de componer f con g.

### Ejemplo

`even . succ`

Esta formada por la composición de *even* que recibe un numero y devuelve un Bool que indica si es par o no, y *succ* que recibe un numero y devuelve el siguiente. Esa composición te devuelve una nueva función que recibe un número y te devuelve el resultado de primero sumarle uno y luego ver si es par:

`(not . even) 2       (espero se entiendan mis flechitas xD)`
`  ^  |  ^    |`
`  |__|  |____|`
` `

Fijate que el 2 se aplica a la función de la derecha y el resultado de eso se aplica a la función de la izquierda. Y ese sería el resultado final de la función.

Tanto lo de la izquierda como lo de la derecha del punto, tienen que ser funciones que reciban 1 parámetro (al menos en paradigmas, lo otro es más complejo). Y a su vez, el **tipo que retorna la función de la derecha, tiene que ser el mismo que el tipo que espera recibir la función de la izquierda**. En este caso, `even` es una función que recibe un *Integral* (ver [typeclasses](typeclasses.html)) y retorna un Bool.

`even :: Integral a => a -> Bool`

y `not` recibe un Bool y retorna otro.

`not :: Bool -> Bool`

Conviene prestar atención a lo siguiente: lo que devuelve `even`, coincide con lo que recibe `not`. Eso hace posible la composición!

Preguntas Frecuentes
--------------------

### ¿Es necesesario que esten los puntos para que sea composicion?

<dl>
<dd>
Porque para mí está implícito que hay composición, si por ejemplo en lugar de:

`impar = not . even`

pongo

`impar = not even`

</dl>
**Sí, es necesario.** En el segundo ejemplo estas aplicando even a not, y not espera un Bool, no una función y por ende, ni compila por error de tipos.

En cambio, cuando hacés

`not . even`

estás armando una **nueva función** que recibe un número, ve si es par y después lo niega. La forma de armar la función que querés, según el segundo ejemplo sería:

`not (even 3)`

Y en el segundo se aplicaría así:

`(not . even) 3`

Cuándo usar composición
-----------------------

¿Cuál es la diferencia entre estas dos definiciones?

`impar n = not (even n)`
`impar = not ◦ even`

En un primer nivel de análisis, ambas definiciones son **equivalentes**.

Sin embargo, si analizamos solamente las expresiones a la derecha del igual encontramos que

`not (even n) `

y

`not . even`

**son distintas**: la primera *denota* un *valor booleano* (True o False) mientras que la segunda denota *una función*.

Esta segunda expresión es más poderosa que la primera, porque me permite que yo la use en funciones de orden superior, por ejemplo

`filter (not.even) [1,2,3,4,5,6,7,8,9]`

En cuanto a la definición de función , no tiene grandes ventajas sobre salvo que nos ayuda a entrenarnos en el uso de la composición, que después podemos utilizar para otras cosas.

Aunque en funciones más complejas puede tener su utilidad, para un ejemplo de esto vean: [Append como "foldr f a"](Append_como_"foldr_f_a" "wikilink").

Errores comunes
---------------

### Ejemplo

Supongamos una lista de alumnos representados con tuplas de tipo (nombre::String, nota::Int). Queremos obtener los nombres de los alumnos aprobados (nota &gt;= 4).

Podemos suponer además la existencia de las funciones:

`nombre (nom, _) = nom`
`aprobado (_, not) = not >= 4`

`nombres alumnos = map nombre alumnos`
`aprobados alumnos = filter aprobado alumnos`

Un error que veo con frecuencia es hacer:

`nombreDeAprobados alumnos = nombres . aprobados alumnos`

La composición es una operación *entre funciones* esto quiere decir que a ambos lados del "." **debe** haber una función. ¿Qué hay a cada lado del "." en este caso:

-   , no hay problema: es una función (de listas de alumnos/tuplas en listas de nombres);

-   ... sí hay problema! No es una función, es una lista de alumnos.

Diciéndolo "en fácil": le tiene que faltar un parámetro, si yo a una función le aplico todos los parámetros deja de ser una función y pasa a ser un valor "simple". *Tal vez sea interesante ver el efecto de la [currificación](currificacion.html) en Haskell.*

### Correcciones posibles

Sin composición  

`aprobados alumnos = nombres (aprobados alumnos)`

  
Es decir uso *aplicación* en lugar de composición (ojo, esto funciona pero si en estamos en un parcial y se desea evaluar que el alumno sepa composición... ahí no están usando composición entonces puede no ser suficiente como solución al ejercicio).

Con composición  

`nombreDeAprobados = nombres . aprobados`

  
Claramente son funciones las dos expresiones a ambos lados del ".". (Notese que a la derecha del "=" también hay un parámetro menos.)

Pueden encontrar otro ejemplo sobre esta clase de errores en [ Errores con composición y aplicación parcial](errores-comunes-al-comenzar-a-trabajar-con-haskell-composicion-y-aplicacion-parcial.html)

### Composición vs. Aplicación

Para terminar de entenderlo recuerden la matemática, ¿es lo mismo que ? Claramente si g es una función yo no puedo hacer . Por otro lado, si en lugar de una función g tuviera un valor real x, entonces puedo hacer pero no .

### Algunos detalles técnicos

Si bien en general intentamos concentrarnos en los conceptos y no prestar tanta atención al conocimiento en sí del lenguaje; para poder expresar correctamente una composición en Haskell es necesario comprender correctamente algunos detalles de la sintaxis del Haskell:

1.  Si pongo un "." es composición, si no es aplicación.
      
    Los dos conceptos son bien distintos y es muy importante comprender la diferencia; por lo tanto es necesario ser bien explícito sobre cuándo se esta queriendo utilizar uno u otro. En criollo, se tiene que notar dónde hay un punto y dónde no.

2.  El operador de composición tiene mayor precedencia que todos los demás. (Ver [Aplicación Parcial](aplicacion-parcial.html))
      
    Por lo tanto la expresión debe leerse como .

    De la misma manera si yo pongo se lee como y es incorrecto por lo expresado en el punto anterior.

    Una alternativa posible es alterar la precedencia explícitamente usando paréntesis, por ejemplo ; donde "primero" se componen las funciones, eso produce una nueva función y a esa nueva función le aplico la lista como parámetro.

    Para más información puede leer: [Precedencia de los operadores más comunes en Haskell](precedencia-de-los-operadores-mas-comunes-en-haskell.html) y [Cuándo usar paréntesis](cuando-usar-parentesis.html)


