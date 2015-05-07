Estrategias de evaluación
-------------------------

Introducción
============

La operación que realizamos en funcional es aplicar funciones, la idea del tema que vamos a tratar a continuación es saber qué se tiene que tener en cuenta para determinar el orden en en que aplicarán las funciones de una expresión.

Primer ejemplo
==============

`  masUno x = x + 1`

La expresión masUno (2\*3) puede ser evaluada de la siguiente forma

`  masUno (2*3)`

-   aplicamos \*

`  masUno 6`

-   aplicamos masUno

`  6 + 1`

-   aplicamos +

`  7`

Alternativamente podemos evaluar la misma expresión pero aplicando las funciones en el orden inverso

`  masUno (2*3)`

-   aplicamos masUno

`  (2*3) + 1`

-   aplicamos \*

`  6 + 1`

-   aplicamos +

`  7`

No importa el orden en que apliquemos las funciones vamos a llegar al mismo resultado final. Esto no solo vale para ejemplos sencillos sino que se cumple siempre en Haskell.

Esta propiedad no se cumple en la mayoría de los "lenguajes imperativos" (Pascal, C, Smalltalk, Java, C\#, etc.), veamos un ejemplo en Smalltalk:

Si tenemos la expresión n + (n := 1) y n empieza apuntando a 0.

Si empezamos a evaluar de izquierda a derecha

` n + (n := 1)`

-   aplicamos n

` 0 + (n := 1)`

-   aplicamos :=

` 0 + 1`

-   aplicamos +

` 1`

Si empezamos a evaluar de derecha a izquierda

` n + (n:= 1)`

-   aplicamos :=

` n + 1`

-   aplicamos n

` 1 + 1`

-   aplicamos +

` 2`

Como se puede observar, si evaluamos las expresiones con distintas estrategias obtenemos resultados distintos; esto sucede porque las operaciones involucradas no tienen [ transparencia referencial](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html) en este caso particular debido a la introducción de una [ asignación destructiva](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html).

Estrategias básicas
===================

A una expresión que consta de una función aplicada a uno o más parámetros y que puede ser "reducida" aplicando dicha función la vamos a llamar Redex (Reducible Expression). Se le dice reducción al hecho de aplicar la función no necesariamente vamos a obtener una expresión "más corta" como veremos más adelante. Consideremos la función mult que tiene como dominio una tupla de 2 números

` mult (x,y) = x * y`

Si queremos reducir la expresión mult (1+2,2+3) está expresión contiene 3 redexs

-   1. 1+2 (la función + aplicada a 2 parámetros)
-   2. 2+3 (la función + aplicada a 2 parámetros)
-   3. mult (1+2,2+3) (la función mult aplicada a 1 parámetro que es una tupla)

Si queremos evaluar la expresión ¿qué estrategia usamos?

De adentro hacia afuera
-----------------------

También conocida como call-by-value

Una de las estrategias más comunes es comenzar desde adentro hacia afuera (innermost evaluation), esta estrategia elige el redex que está "más adentro" entendiendo por esto al redex que no contiene otro redex. Si existe más de un redex que cumple dicha condición se elige el que está más a la izquierda. Vamos al ejemplo

` mult (1+2,2+3)`

-   aplicamos el primer +

` mult (3,2+3)`

-   aplicamos el +

` mult (3,5)`

-   aplicamos mult

` 3 * 5`

-   aplicamos \*

` 15`

Esta estrategia me asegura que los parámetros de una función están completamente evaluados antes de que la función sea aplicada. Por eso se dice que los parámetros se pasan por valor.

De afuera hacia adentro
-----------------------

También conocida como call-by-name

Otra de las estrategias más comunes es comenzar desde afuera hacia adentro (outtermost evaluation), esta estrategia elige el redex que está "más afuera" entendiendo por esto al redex que no esta contenido en otro redex. Si existe más de un redex que cumple dicha condición se elige el que está más a la izquierda. Vamos al ejemplo

mult (1+2,2+3)

-   aplicamos mult

` (1+2) * (2+3)`

-   aplicamos el primer + (Si seguimos lo que dijimos arriba deberíamos aplicar primero el \* pero vamos a explicar porque no lo hacemos más abajo)

`  3 * (2+3)`

-   aplicamos +

`  3 * 5`

-   aplicamos \*

`  15`

Usando esta estrategia las funciones se aplican antes que los parámetros sean evaluados. Por esto se dice que los parámetros se pasan por nombre. Nota: Hay que tener en cuenta que muchas funciones que ya vienen con Haskell requieren que sus parámetros estén evaluados antes de que la función sea aplicada, incluso cuando usamos la estrategia "de afuera hacia adentro". Por ejemplo, el operador \* y el + no pueden ser aplicados hasta que sus dos parámetros hayan sido evaluados a números. A las funciones que cumplen con esta propiedad las vamos a llamar funciones estrictas. Funciones estrictas que nos van a interesar a nosotros:

-   Operaciones aritméticas (+,\*,/,etc.)
-   Pattern-Matching (sobre listas, tuplas, etc.)

Evaluaciones que no terminan
============================

Tengan en cuenta la siguiente definición

` inf = 1 + inf`

Intentar reducir la expresión inf siempre nos va a dar como resultado una expresión más y más grande (independientemente de la estrategia de evaluación que usemos)

` inf`

-   aplicamos inf

` 1 + inf`

-   aplicamos inf (porque + es estricta)

` 1 + (1 + inf)`

-   aplicamos inf (porque + es estricta)

....

` 1 + (1 + (1 + (1 + (1 + (1 + .... + inf )))))`

Por ende, está evaluación nunca terminaría.

Sabiendo que

` fst (x,_) = x`

Consideremos la expresión fst (0,inf)

Usando la estrategia call-by-value

` fst (0,inf)`

-   aplicamos inf

` fst (0, 1 + inf )`

-   aplicamos inf

` fst (0, 1 + (1 + inf) )`

-   aplicamos inf

` fst (0, 1 + (1 + (1 + inf) ) )`

-   aplicamos inf

...

Usando call-by-value la evaluación de la expresión no termina.

Usemos call-by-name:

` fst (0,inf)`

-   aplicamos fst

` 0`

Usando call-by-name la expresión se evalúa por completo con solo una reducción. En este ejemplo se puede ver que ciertas expresiones que pueden no terminar cuando se evalúan con la estrategia call-by-value pueden terminar cuando se usa la estrategia call-by-name.

De forma más general: Si existe alguna secuencia de evaluación que haga terminar la evaluación de la expresión entonces con la estrategia call-by-name también se termina la evaluación y se produce el mismo resultado final.

Corolario: si te es mucho muy importante que una expresión termine la estrategia que querés usar es call-by-name

Lazy Evaluation
===============

Visión técnica
--------------

Si tenemos la siguiente definición

` alCuadrado x = x * x`

Vamos a evaluar la expresión alCuadrado (2\*3) usando call-by-value

` alCuadrado (1+2)`

-   aplicamos +

` alCuadrado 3`

-   aplicamos alCuadrado

` 3 * 3`

-   aplicamos \*

` 9`

Ahora vamos a evaluar la misma expresión usando call-by-name

` alCuadrado (1+2)`

-   aplicamos alCuadrado el primer +

` (1+2) * (1+2)`

-   aplicamos el +

` 3 * (1+2)`

-   aplicamos el \*

` 3 * 3`

-   aplicamos el \*

` 9`

Llegamos la mismo resultado pero en el segundo ejemplo realizamos una reducción más (4 reducciones vs 3 reducciones).

Con call-by-name la expresión (1+2) se evaluó dos veces.

Corolario: cuando usamos call-by-value los parámetros son evaluados una y solo una vez; cuando usamos call-by-name el mismo parámetro puede llegar a ser evaluado más de una vez.

Para evitar este quilombo en vez de tener la expresión (1+2) vamos a tener un "puntero a la expresión" llamémoslo p.

`alCuadrado (1+2)`

-   aplicamos alCuadrado

`let p = (1+2) in p * p`

-   aplicamos +

`let p = 3 in p * p`

-   aplicamos \*

` 9`

Cualquier reducción que se haga en una expresión se va a conocer automáticamente por los punteros a dicha expresión. Al uso de punteros para compartir expresiones que representan la mismo parámetro lo vamos a llamar Sharing. Al uso de la estrategia call-by-name más el Sharing lo vamos a llamar Lazy Evaluation (esta es la estrategia que usa Haskell). El Sharing nos asegura que usar Lazy Evaluation nunca requiera más pasos que la estrategia call-by-value.

Visión operativa
----------------

A efectos de resumir lo que vimos hasta ahora vamos a entender lo siguiente ...

**Lazy Evaluation**: con esta estrategia los parámetros solo se resuelven cuando son necesarios (y son evaluados solo lo necesario). También conocida como **evaluación perezosa** o **diferida**.

A la estrategia call-by-value (y sus variantes) también se las conoce como Eager Evaluation. **Eager Evaluation**: con esta estrategia los parámetros tienen que resolverse antes de aplicar la función. También conocida como **evaluación ansiosa**.

Estructuras infinitas
---------------------

Pensemos en la siguiente definición

`  unos = 1 : unos`

(A partir de ahora vamos a pensar que evaluamos todo en Haskell así que la estrategia que usamos es Lazy Evaluation)

` unos`

-   aplicamos unos

` 1 : unos`

-   aplicamos unos

` 1 : ( 1 : unos )`

-   aplicamos unos

...

En Haskell

`  > unos`
`   [1,1,1,1,1,1,1........`

Como se puede ver la evaluación de unos no termina. A pesar de esto podemos usar la expresión unos dentro de nuestro programa y aplicarla a otras funciones. Por ejemplo

Siendo head (x:\_) = x y la expresión head unos

` head unos`

-   deberíamos aplicar head pero como head me fuerza a tener la lista separada en cabeza:cola tenemos que evaluar unos por el pattern-matching

` head (1:unos)`

-   aplicamos head

` 1`

Con este ejemplo podemos ver que unos no es una lista infinita sino potencialmente infinita, si aplicamos sobre ella funciones que no la fuerzan a evaluarse por completo la computación termina (eso sonó apocalíptico). La potencia de Lazy Evaluation está en que la expresión unos se evalúa solo lo necesario para que pueda usarla la función que la recibe como parámetro.

Listas infinitas
----------------

Ya vimos la lista de unos que es "infinita", ahora veamos como hacer una lista que tenga todos los números naturales

` naturalesDesde x = x : naturalesDesde (x+1)`

` > naturalesDesde 1`
`   [1,2,3,4,5,6,7,8,9,...........`

Haskell trae un atajo para esto

` naturalesDesde x = [x..]`

También sirve para hacer listas con alguna condición entre dos de sus elementos consecutivos

` > [1,3..]`
`   [1,3,5,7,9,11,.........`

Ejemplos
========

Dada la siguiente definición de take

` take 0 _ = []`
` take _ [] = []`
` take n (x:xs) = x : (take (n-1) xs)`

` take 3 [1..]`

-   aplicamos ..

` take 3 (1:[2..])`

-   aplicamos take - 3ra línea

` 1 : (take 2 [2..])`

-   aplicamos ..

` 1 : (take 2 (2:[3..]))`

-   aplicamos take - 3ra línea

` 1 : ( 2 : (take 1 [3..]))`

-   aplicamos ..

` 1 : ( 2 : (take 1 (3:[4..])))`

-   aplicamos take - 3ra línea

` 1 : ( 2 : ( 3 : (take 0 [ 4.. ]))))`

-   aplicamos take - 1ra línea

` 1 : ( 2 : ( 3 : [] ))) = [1,2,3]`

Vamos a otro ejemplo

` take 3 [4+5,2/0,3*2]`

-   aplicamos el + (porque dice x: en take 3ra línea)

` take 3 [9,2/0,3*2]`

-   aplicamos take - 3ra línea

` 9 : take 2 [2/0,3*2]`

-   aplicamos /

` 9 : ERROR DIVISON BY ZERO !!!`

Dada la definición de (!!)

`(!!) 0 (x:_) = x`
`(!!) n (_:xs) = (!!) (n-1) xs`

`(!!) 2 [4+5,2/0,3*2]`

-   aplicamos el !! (no es necesario aplicar el + porque en (!!) dice (\_:xs) )

`(!!) 1 [2/0,3*2]`

-   aplicamos el !! (no es necesario aplicar la / por lo anterior)

`(!!) 0 [3*2]`

-   aplicamos \* (porque la primer línea de !! lo pide)

`(!!) 0 [6]`

-   aplicamos !!

`6`

Supongamos que hacemos esta consulta:

` > head (filter (3<) [1..])`

Si bien la expresión `filter` `(3<)` `[1..]` no termina (seguiría buscando cuáles son mayores a 3 infinitamente), como lo que primero se evalúa es el head y se difiere la ejecución del filtrado, la ejecución va a terminar en cuanto el filter encuentre su primer elemento que pertenezca a la solución que es el 4.

Es importante notar que en este otro caso:

` > head (filter (<0) [1..])`

La evaluación nunca termina por más que se use head que era lo que antes acotaba la ejecución, ya que nunca se va a encontrar el primer elemento que cumpla la condición a diferencia del caso anterior.
