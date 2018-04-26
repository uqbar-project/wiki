---
layout: article
title: Orden superior
---

### Generalidades

Llamamos a una determinada operación de orden superior si la misma recibe otra operación (comportamiento) por parámetro, siendo capaz de ejecutarla internamente.

Al usar orden superior tenemos las siguientes ventajas:

-   Puedo aislar y **reutilizar comportamiento común**.
-   Puedo partir mi problema, **separando responsabilidades**, entre el código que tiene orden superior, y el comportamiento parametrizado.
-   Puedo tener un código con partes **incompletas**, esperando rellenarlos **pasando comportamiento por parámetro**, y no sólo datos.
-   ¡Puedo generar **[abstracciones](abstraccion.html) más jugosas**! Más allá de las abstracciones de orden superior que ya me proveen los lenguajes, la mayoría de los mismos me dan la posibilidad de armar mi propio comportamiento de orden superior (En la materia lo vemos en Haskell más directamente, aunque se puede también en SWI-Prolog).
-   Con abstracciones más adecuadas, con responsabilidades repartidas, y sin repetición de lógica, genero un código más [**expresivo** (porque en general es más fácil de leer), y más **declarativo** (porque en general al usar orden superior oculto detalles algorítmicos)](declaratividad-vs--expresividad.html)

### Paradigma Funcional

Tanto en matemática como en informática, se definen las funciones de orden superior como funciones que reciben funciones por parámetro o bien devuelven una función como resultado.

La función filter (en haskell y otros lenguajes funcionales) es un ejemplo común de este tipo de funciones ya que recibe por parámetro una función f y una lista y retorna una nueva lista que contiene aquellos elementos que al ser aplicados como parámetros a f devuelven verdadero. La función f debe tener entonces aridad 1 y devolver un valor booleano.

`filter :: (a -> Bool) -> [a] -> [a]`

Otras funciones comunes son map, [fold](fold.html), funciones de ordenamiento o búsqueda, composición de funciones (.), flip, etc (pueden consultar sus tipos en el intérprete con :t).

Ejemplos comunes del análisis matemático que es más difícil de ver en los lenguajes de programación son la derivada y la integral (Vemos al "proceso de derivar" como una función que recibe otra función cualquiera y devuelve su derivada).

En el [Cálculo lambda](http://es.wikipedia.org/wiki/C%C3%A1lculo_lambda) no tipado todas las funciones son de orden superior. En el cálculo lambda con tipado, desde el cual la mayoría de los lenguajes funcionales se derivan, las funciones de orden superior son generalmente aquellas cuyos tipos de parámetros "contienen una o más flechitas".

En matemática a las funciones que retornan funciones se las conoce como operadores o *functionals*. Un ejemplo común es el cálculo derivativo, ya que mapea una función a otra función.

#### ¿La aplicación parcial es también orden superior?

En primer lugar, recordemos que en Haskell las funciones están [currificadas](http://es.wikipedia.org/wiki/Currificaci%C3%B3n). La [Aplicación Parcial](aplicacion-parcial.html) no es una función, lo que sí podemos analizar es la [Aplicación](aplicacion.html) y lo importante es qué recibe y qué devuelve, eso se ve cuando se analiza el tipo de la función. Existe una función que nos va a servir para realizar este análisis llamada $ que lo que hace es aplicarle un parámetro a una función.

```Haskell
($) :: (a -> b) -> a -> b
($) funcion parametro = funcion parametro
```

Si el primer parámetro de la función $ es una función de aridad mayor a 1 (por ejemplo, mod) y la aplicamos de la siguiente forma:

`mod $ 10`

El resultado será una función:

`mod $ 10 :: Integral a => a -> a`

La aplicación, entonces es una función de orden superior ya que recibe una función por parámetro y si la misma no queda totalmente aplicada también retorna una función.

### Paradigma Lógico

Decimos que un predicado es de Orden Superior si este recibe como argumento otro predicado. Algunos ejemplos son [not](paradigma-logico---negacion.html), [findall](paradigma-logico---listas--como-obtener-todas-las-respuestas--juntas--.html) y [forall](paradigma-logico---el-forall.html). Hacer nuestros propios predicados de orden superior no es tan natural como resulta en funcional, sin embargo Prolog nos permite hacerlo mediante el predicado [call](como-hacer-predicados-de-orden-superior.html).

### Paradigma Estructurado

En el paradigma estructurado, una porción de código puede alcanzar resultados algorítmicos como si fuesen obtenidos a través de funciones de orden superior, ejecutando código dinámicamente (a veces denominadas operaciones *"Eval"* o *"Execute"*) durante la evaluación. Desafortunadamente hay limitaciones al alcance del mismo:

- El código en el argumento a ser ejecutado usualmente no posee tipado estático; estos lenguajes generalmente dejan relegado al tipado dinámico la determinación de la seguridad y la buena disposición del código a ser ejecutado.

- El parámetro es usualmente provisto como un *String*, cuyo valor no puede ser conocido hasta el momento de ejecución. Este string debe ser o bien compilado durante la ejecución del programa (usando compilación *just-in-time*) o bien evaluado durante la interpretación, causando un *overhead* adicional y usualmente generando código menos eficiciente.

También pueden ser utilizadas *Macros* para lograr algunos de los efectos del orden superior. No obstante ello, estas macros no pueden evitar fácilmente el problema de la captura de variables; también resultan en grandes cantidades de código duplicado, el cual puede ser más difícil de optimizar para un compilador. Generalmente las Macros no son fuertemente tipadas, aunque pueden producir código fuertemente tipado.

Si les interesa profundizar en el uso de orden superior en el lenguaje C, pueden leer [el siguiente apunte](https://docs.google.com/document/d/1GZOTwkO02X194hlLBSECIhFDB_P1VuIwlTy5FS8ASMY/) realizado por ayudantes de la cátedra.

### ¿Y los objetos?

Esta me gustó más, ahí vamos, en objetos no tiene sentido reconocer ordenes, porque lo natural es que si la operacion es el mensaje (recordar valores y operaciones), entonces tanto el receptor del mensaje como los parametros son siempre objetos.

Entonces, estoy estudiando para el final, que tengo que entender:
- que es eso de "orden": un X que puede admitir otro X como parametro. 
- digo que reconozco dos ordenes, si eso es una característica especial que quiero remarcar, si no... lo que pasa es que no tiene sentido la distinción de órdenes en este contexto 
- para cada paradigma: si tiene sentido decir algo asi como "orden superior", y en que casos 
- para qué uso la idea superior, en particular en funcional. Una vez que entendi eso, que herramienta que vimos de Wollok me sirve para un propósito similar.
  - En Haskell: `filter (>3) unaLista`
  - En Wollok: `unaLista.filter({elemento => elemento > 3})`

Está claro que ambos ejemplos sirven para lo mismo, pero ¿qué es lo que recibe por parámetro el filter de la solución objetosa? Es un bloque, y los bloques son objetos.

En general no aplicamos el concepto de orden superior a los objetos porque si la definición fuera "un obeto que recibe otro objeto por parámetro" eso incluye a todos los objetos que reciban algún parámetro en algún mensaje porque.... todos son objetos. Podés pensar que todos son de orden superior, si preferís, pero no podés clasificar a los objetos en "órdenes".

Por otro lado me gustaría disentir con la idea de relacionar muy fuertemente a los bloques con orden superior, es cierto que estoy pasando "comportamiento" por parámetro. Pero, como decía, eso pasa muchas veces en objetos, entonces hacer una mención especial a los bloques no me parece. En algún momento nosotros mismos lo contamos en esos términos, pero bueno, evolucionamos. Igualmente esa respuesta no se consideraría errónea en un final, pero sí quiero comenzar a transmitirla de esta forma que me parece más adecuada.

Creo que la diferencia del bloque con el resto de los objetos es la creación de comportamiento ad-hoc, para un fin específico, los uso una sola vez y no les pongo nombre. Ese efecto se asemeja a otros elementos del funcional, como ser las [Expresiones lambda](expresiones-lambda.html) o la [Aplicación Parcial](aplicacion-parcial.html), que me permiten armar "en el aire" funciones con esas características (fíjense que también pasa en funcional, que las expresiones lambda o la aplicación parcial son útiles para combinarlas con las funciones de orden superior).

Volviendo al tema principal, el chiste del orden superior es que yo paso algo más complejo que un número, una lista o una estructura (functor, tupla, etc) por parámetro. Al pasar un predicado o función yo paso "comportamiento" por parámetro, el conocimiento de cómo resolver el problema ahora no se divide en datos y algoritmo, sino que la función de orden superior tiene parte del "algoritmo" y la otra parte viene por parámetro. (Nótense las comillas al usar las palabras "algoritmo" y "comportamiento" en los paradigmas declarativos.) Eso le da un poder muy piola a lo que construimos. En fin, ¿cómo llevamos eso de nuevo a objetos? Yo creo que se aprovechan beneficios similares a los del orden superior en el momento en que los objetos que interactúan (el receptor del mensaje y el parámetro) tienen dividido el comportamiento en esa forma, es decir, que el objeto receptor le pide al otro cosas que no sean solamente devolver un dato sino que tengan otra complejidad. Claro, cuando uso un bloque eso pasa siempre, y por eso es tentador establecer la relación. Pero si abrimos un poco la cancha, creo que es fácil ver que ocurre casi siempre que tengamos delegación y responsabilidades bien repartidas.

### Links Relacionados

-   [Lambdas en Java 8](lambdas-en-java-8.html)
-   [Introduction to higher-order functions](http://www.cs.aau.dk/~normark/prog3-03/html/notes/higher-order-fu_themes-intr-section.html)
-   [Higher-order function](http://en.wikipedia.org/wiki/Higher-order_function)
-   [Closures and Higher-Order Functions](http://weblog.raganwald.com/2007/01/closures-and-higher-order-functions.html)
-   [Higher-order functions and variational calculus](high-order-functions-and-variational-calculus.html)
-   [Boost Lambda Library for C++](http://boost.org/doc/html/lambda.html)

