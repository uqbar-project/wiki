---
layout: article
title: Composicion
---

Qué es composición?
-------------------

¿Qué queremos decir con 'Componer'?  
Lo mismo que con composición de funciones en matemática.

fog(x) es lo mismo que f(g(x))

Es decir que lo que devuelve g, hay que aplicárselo a f. A tener en cuenta, tanto f como g tienen que ser funciones, no podemos componer una función con un 2, o un 2 con una lista.

> Sólo podemos componer funciones

Para componer dos funciones en Haskell, por ejemplo f y g, se hace así

```haskell
f . g
```

eso arma una nueeeva función, resultado de componer f con g.

### Ejemplo

```haskell
even . succ
```

Esta formada por la composición de *even* que recibe un numero y devuelve un Bool que indica si es par o no, y *succ* que recibe un numero y devuelve el siguiente. Esa composición te devuelve una nueva función que recibe un número y te devuelve el resultado de primero sumarle uno y luego ver si es par:

```
(not . even) 2       (espero se entiendan mis flechitas xD)
  ^  |  ^    |
  |__|  |____|
```

Fijate que el 2 se aplica a la función de la derecha y el resultado de eso se aplica a la función de la izquierda. Y ese sería el resultado final de la función.

Tanto lo de la izquierda como lo de la derecha del punto, tienen que ser funciones que reciban 1 parámetro (al menos en paradigmas, lo otro es más complejo). Y a su vez, el **tipo que retorna la función de la derecha, tiene que ser el mismo que el tipo que espera recibir la función de la izquierda**. En este caso, `even` es una función que recibe un *Integral* (ver [typeclasses](typeclasses.html)) y retorna un Bool.

```haskell
even :: Integral a => a -> Bool
```

y `not` recibe un Bool y retorna otro.

```haskell
not :: Bool -> Bool
```

Conviene prestar atención a lo siguiente: lo que devuelve `even`, coincide con lo que recibe `not`. Eso hace posible la composición!

Preguntas Frecuentes
--------------------

### ¿Es necesesario que esten los puntos para que sea composicion?

Porque para mí está implícito que hay composición, si por ejemplo en lugar de:

```haskell
impar = not . even
```

pongo

```haskell
impar = not even
```

**Sí, es necesario.** En el segundo ejemplo estas aplicando even a not, y not espera un Bool, no una función y por ende, ni compila por error de tipos.

En cambio, cuando hacés

```haskell
not . even
```

estás armando una **nueva función** que recibe un número, ve si es par y después lo niega. La forma de armar la función que querés, según el segundo ejemplo sería:

```haskell
not (even 3)
```

Y en el segundo se aplicaría así:

```haskell
(not . even) 3
```

Cuándo usar composición
-----------------------

¿Cuál es la diferencia entre estas dos definiciones?

```haskell
impar n = not (even n)
impar = not ◦ even
```

En un primer nivel de análisis, ambas definiciones son **equivalentes**.

Sin embargo, si analizamos solamente las expresiones a la derecha del igual encontramos que

```haskell
not (even n)
```

y

```haskell
not . even
```

**son distintas**: la primera *denota* un *valor booleano* (True o False) mientras que la segunda denota *una función*.

Esta segunda expresión es más poderosa en cuanto a que nos permite hacer más cosas que la primera, ya que la construcción de la función independiente de su aplicación sirve, por ejemplo, para trabajar con [funciones de orden superior](orden-superior.html).

En cuanto a la definición de función, no tiene grandes ventajas sobre salvo que nos ayuda a entrenarnos en el uso de la composición, que después podemos utilizar para otras cosas. Sin embargo, una de las virtudes asociadas si se reemplazan muchas aplicaciones anidadas por composición de funciones podría implicar un código más limpio, porque la sintaxis de Haskell está diseñada de modo que eso suceda.

Errores comunes
---------------

### Ejemplo

Supongamos una lista de alumnos de los cuales se sabe su nombre y su nota. Queremos obtener los nombres de los alumnos aprobados.

Podemos suponer además la existencia de las funciones:

```haskell
nombres :: [Alumno] -> [String]
aprobados :: [Alumno] -> [Alumno]
```

Un error que veo con frecuencia es hacer:

```haskell
nombreDeAprobados alumnos = nombres . aprobados alumnos
```

La composición es una operación *entre funciones* esto quiere decir que a ambos lados del "." **debe** haber una función. ¿Qué hay a cada lado del "." en este caso:

-  `nombres` , no hay problema: es una función (de listas de alumnos/tuplas en listas de nombres);

-  `aprobados alumnos` ... sí hay problema! No es una función, es una lista de alumnos.

Diciéndolo "en fácil": debo componer funciones, no vale componer valores. Si yo a una función le aplico todos los parámetros deja de ser una función y pasa a ser un valor "simple". En este caso, `aprobados` es una función, mientras que `aprobados alumnos` es un valor, y como tal no se puede componer. *Tal vez sea interesante ver el efecto de la [currificación](currificacion.html) en Haskell.*

### Correcciones posibles

Sin composición  

```haskell
aprobados alumnos = nombres (aprobados alumnos)
```

  
Es decir uso *aplicación* en lugar de composición (ojo, esto funciona pero si en estamos en un parcial y se desea evaluar que el alumno sepa composición... ahí no están usando composición entonces puede no ser suficiente como solución al ejercicio).

Con composición  

```haskell
nombreDeAprobados = nombres . aprobados
```
  
Claramente son funciones las dos expresiones a ambos lados del ".". (Notese que a la derecha del "=" también hay un parámetro menos.)

Pueden encontrar otro ejemplo sobre esta clase de errores en [ Errores con composición y aplicación parcial](errores-comunes-al-comenzar-a-trabajar-con-haskell.html)

### Composición vs. Aplicación

Para terminar de entenderlo recuerden la matemática, ¿es lo mismo que ? Claramente si g es una función yo no puedo hacer . Por otro lado, si en lugar de una función g tuviera un valor real x, entonces puedo hacer pero no .

### Algunos detalles técnicos

Si bien en general intentamos concentrarnos en los conceptos y no prestar tanta atención al conocimiento en sí del lenguaje; para poder expresar correctamente una composición en Haskell es necesario comprender correctamente algunos detalles de la sintaxis del Haskell:

1. Si pongo un "." es composición, sino es aplicación. Los dos conceptos son bien distintos y es muy importante comprender la diferencia; por lo tanto es necesario ser bien explícito sobre cuándo se esta queriendo utilizar uno u otro. En criollo, se tiene que notar dónde hay un punto y dónde no.

2. El operador de composición tiene poca precedencia. (Ver [Aplicación Parcial](aplicacion-parcial.html))  Por lo tanto la expresión `not . even 3` debe leerse como `not . (even 3)` (y por lo tanto es incorrecta). Una alternativa posible es alterar la precedencia explícitamente usando paréntesis, por ejemplo `(not.even) 3`; donde _primero_ se componen las funciones, eso produce una nueva función, y a esa nueva función le aplico el 3 como parámetro.

Para más información puede leer: [Precedencia de los operadores más comunes en Haskell](precedencia-de-los-operadores-mas-comunes-en-haskell.html) y [Cuándo usar paréntesis](cuando-usar-parentesis.html)
