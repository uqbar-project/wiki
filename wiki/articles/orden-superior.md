### Paradigma Funcional

In mathematics and computer science, higher-order functions or functionals are functions which do at least one of the following:

`   * take one or more functions as an input`
`   * output a function`

In mathematics these are also known as operators or functionals. The derivative in calculus is a common example, since it maps a function to another function.

In the untyped lambda calculus, all functions are higher-order; in a typed lambda calculus, from which most functional programming languages are derived, higher-order functions are generally those with types containing more than one arrow. In functional programming, higher-order functions that return other functions are said to be curried.

The map function found in many functional programming languages is one example of a higher-order function. It takes as arguments a function f and a list of elements, and as result, returns a new list with f applied to each element from the list. Another very common kind of higher-order function in those languages which support them are sorting functions which take a comparison function as a parameter, allowing the programmer to separate the sorting algorithm from the comparisons of the items being sorted. The C standard function, qsort, is an example of this.

Other examples of higher-order functions include fold, function composition, integration, and the constant-function function λx.λy.x.

#### ¿La aplicación parcial es también orden superior?

Con la aplicacion parcial podes obtener nuevas funciones, y siempre a patir de la misma, pero no es orden superior. Lo importante es que recibe y que devuelve, eso se ve cuando se analiza el tipo de la funcion.

Por ejemplo, + , recibe dos numeros y devuelve otro; yo puedo aplicar parcialmente asi (5+); y obtuve una nueva funcion; pero + no es de orden superior.

### Paradigma Lógico

Decimos que orden superior es porque para el p. logico: un predicado es de Orden Superior si este recibe como argumento otro predicado.

### ¿Y los objetos?

Esta me gustó más, ahi vamos, en objetos no tiene sentido reconocer ordenes, porque lo natural es que si la operacion es el mensaje (recordar valores y operaciones), entonces tanto el receptor del mensaje como los parametros son siempre objetos.

Entonces, estoy estudiando para el final, que tengo que entender: - que es eso de "orden": un X que puede admitir otro X como parametro. - digo que reconozco dos ordenes, si eso es una caracteristica especial si quiero remarcar, si no, lo que pasa es que no tiene sentido la distincion de ordenes - para cada paradigma: si tiene sentido decir algo asi como "orden superior", y en que casos - para qué uso la idea superior, en particular en funcional. Una vez que entendi eso, que herramienta que vimos de Smalltalk me sirve para un proposito similar.

en haskell&gt; filter (&gt;3) unaLista en smalltalkt&gt; unaLista select: \[ :each | each &gt; 3\]

Qué analogías podés establecer entre estas líneas? A qué se parece el filter? y la función &gt;3?

En general no aplicamos el concepto de orden superior a los objetos porque si la definición fuera "un obeto que recibe otro objeto por parámetro" eso incluye a todos los objetos que reciban algún parámetro en algún mensaje porque.... todos son objetos. Podés pensar que todos son de orden superior, si preferís, pero no podés clasificar a los objetos en "ordenes".

Por otro lado me gustaría disentir con la idea de relacionar muy fuertemente a los bloques con orden superior, es cierto que estoy pasando "comportamiento" por parámetro. Pero, como decía, eso pasa muchas veces en objetos, entonces hacer una mención especial a los bloques no me parece. (Sí, ya sé, en algún momento yo mismo lo conté en esos términos, pero bueno, evolucionamos. Igualmente esa respuesta no se consideraría errónea en un final, pero sí quiero comenzar a transmitirla de esta forma que me parece más adecuada.) Creo que la diferencia del bloque con el resto de los objetos es la creación de comportamiento ad-hoc, para un fin específico, los uso una sola vez y no les pongo nombre. Ese efecto se asemeja a otros elementos del funcional, como ser las expresiones lambda o la aplicación parcial, que me permiten armar "en el aire" funciones con esas características (fíjense que también pasa en funcional, que las expresiones lambda o la aplicación parcial son útiles para combinarlas con las funciones de orden superior).

Volviendo al tema principal, el chiste del orden superior es que yo paso algo más complejo que un número, una lista o una estructura (functor, tupla, etc) por parámetro. Al pasar un predicado o función yo paso "comportamiento" por parámetro, el conocimiento de cómo resolver el problema ahora no se divide en datos y algoritmo, sino que la función de orden superior tiene parte del "algoritmo" y la otra parte viene por parámetro. (Nótense las comillas al usar las palabras "algoritmo" y "comportamiento" en los paradigmas declarativos.) Eso le da un poder muy piola a lo que construimos. En fin, ¿cómo llevamos eso de nuevo a objetos? Yo creo que se aprovechan beneficios similares a los del orden superior en el momento en que los objetos que interactúan (el receptor del mensaje y el parámetro) tienen dividido el comportamiento en esa forma, es decir, que el objeto receptor le pide al otro cosas que no sean solamente devolver un dato sino que tengan otra complejidad. Claro, cuando uso un bloque eso pasa siempre, y por eso es tentador establecer la relación. Pero si abrimos un poco la cancha, creo que es fácil ver que ocurre casi siempre que tengamos delegación y responsabilidades bien repartidas.

### En resumen
