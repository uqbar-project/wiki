---
layout: article
title: Expresiones lambda
---

Son funciones anónimas. Por ej., una función anónima que toma solo un número como argumento y produce el resultado x + x se puede definir como:

`   λx → x + x`

En Haskell:

`λ` se escribe `\` y `→` se escribe `->`;

Esa definición no es más que una función doble, por eso podemos escribir

```Haskell
doble = \x -> x + x
```

A pesar de no tener nombre, pueden usarse como cualquier otra función:

```Haskell
>(\x -> x + x) 2
4
```

Una expresión lambda puede recibir más de 1 parámetro separándolos por espacio dentro de la expresión

```Haskell
--cuentaLoca es una función que recibe 3 parámetros
cuentaLoca = (\x y z -> x * 2 - y + 10 * z)
```

La gracia de poder definir funciones anónimas es que nos permite armar fácilmente una función para usar en el momento. En general cuando más jugo se les saca a las expresiones lambda es para usar [funciones de orden superior](orden-superior.html), sin embargo pueden usarse en cualquier lugar donde se espere una función. Como las expresiones lambda son funciones, las mismas pueden combinarse con otras funciones usando [Composición](composicion.html).

Una de las ventajas que tienen las lambdas por sobre otros mecanismos de crear nuevas funciones cuando las necesito sin tener que definirlas en otro lado, es que los parámetros tienen un nombre y puedo usar ese mismo parámetro tantas veces como quiera dentro de la definición. Si por ejemplo quisiéramos saber si la edad de una persona es menor a 20 o mayor a 60, podría resolverse de la siguiente forma:

```Haskell
> ((\e -> e < 20 || e > 60).edad) persona
```

Algo importante a tener en cuenta es que si no le damos un nombre a nuestras funciones podríamos perder [abstracciones](abstraccion.html) útiles que podrían luego ser utilizadas en otros puntos de nuestro programa, por lo tanto es importante ser criteriosos respecto a si es una buena idea buscar un nombre para nuestra función.

Por lo general, si tengo una forma sencilla de nombrar una determinada lógica que forma parte de una función más grande, lo más probable es que no quiera definir ese pedacito de lógica usando una lambda, sino con una función que se llame como la idea que tenemos en la cabeza. Si no hay un nombre claro asociado a ese pedacito de lógica, lo más probable es que no sea un concepto del dominio que merezca la pena modelar como algo aparte.

Lambdas y Pattern Matching
--------------------------

Algo interesante que se puede hacer con las expresiones lambda es descomponer sus parámetros usando [pattern matching](pattern-matching-en-haskell.html) como cuando definimos funciones normales. Supongamos que dado un alumno modelado con una tupla de tipo (String, \[Int\]) quiero obtener la primer nota que se sacó, se podría resolver de la siguiente forma:

```Haskell
 > (\(_,(nota:_)) -> nota) alumno
 ```

Al usar pattern matching hay que tener en cuenta que si el parámetro que le llega a la lambda no matchea (por ejemplo si en este caso el alumno no tiene notas), al ejecutarse va a tirar un error indicando que los patrones usados no fueron exhaustivos, lo que significa que la función tiene un dominio acotado, posiblemente más de lo que queríamos.

Para este ejemplo no nos interesa contemplar a los alumnos sin notas porque no tenemos una buena respuesta para dar en ese escenario, con lo cual la solución sería correcta.

Uso de lambdas en vez de aplicación parcial
-------------------------------------------

Uno de los conceptos fuertes que existen en el paradigma funcional es el de [Aplicación Parcial](aplicacion-parcial.html), que nos permite crear una función nueva a partir de otra existente cuando nos hace falta para combinarla con otras funciones mediante composición o simplemente pasarla por parámetro para que otra función la evalúe cuando corresponda.

Siempre que usamos aplicación parcial podemos también usar una lambda, por ejemplo:

```Haskell
> map (+1) [1 .. 10]
```

Tiene el mismo resultado que:

```Haskell
> map (\n -> n + 1) [1 .. 10]
```

En casos como este, el uso de aplicación parcial es más interesante que el uso de la expresión lambda. No sólo hacemos lo mismo con menos código sino que demostramos un mayor entendimiento de los conceptos más fuertes del paradigma.

Sin embargo hay casos en los cuales no podemos resolver el problema aplicando parcialmente ([ver: Puedo aplicar parcialmente el segundo parámetro en vez del primero?](aplicacion-parcial.html)) la función que queremos usar que sí justifican el uso de una expresión lambda.
