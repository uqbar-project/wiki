Como su nombre lo indica, la función es el concepto fundamental del paradigma funcional: mientras que, por ejemplo, en el paradigma de objetos, los problemas se resuelven en términos de envío de mensajes entre objetos, en funcional los programas se estructurarán en torno a aplicación de funciones sobre valores. La función, es así, pues, la [computación](computacion.html) característica del paradigma.

Función como caja negra
-----------------------

Una forma simple de pensar una función es como una máquina con una salida y al menos una entrada, capaz de producir un resultado. Decimos que se trata de una caja negra, porque para aquel que la use no tiene acceso al interior de la misma, sino tan solo a sus entradas y salida. Esto nos lleva a que las funciones pueden ser combinadas fácilmente, de diversas formas, tan solo conociendo el tipo de entradas y salidas que posee.

### Función como transformación matemática

Las funciones, sin embargo, no son simples cajas negras, que podrían, por ejemplo, tener memoria de sus entradas anteriores, sino que son transformaciones, en el sentido matemático: para cada entrada aceptable (su dominio), existe un único resultado (imagen), lo cual se conoce como unicidad. Además, para toda entrada del dominio, existe una un sólo resultado, lo que se conoce como existencia.

Por otro lado, las transformaciones matemáticas presentan [transparencia referencial](transparencia-referencial.html)

### Función desde el cálculo lambda

-   Currificada
-   Primitiva del lenguaje
-   Notación en tipado y no tipado

#### Función como valor

#### Valores como funciones

En el caso particular del cálculo lambda no tipado, no solo la función es la unidad fundamental, sino que es la única, y es el único tipo de dato existente, mientras que la aplicación de las mismas es la única estructura de control. Así, por ejemplo, elementos que en los lenguajes tradicionales son primitivas, como los números, el valor true y la estructura de control if y la recursividad, en el cálculo lambda no tipado son derivados de definición y aplicación de funciones. Ejemplos:

===== Definición de if ====

`True := λt. λf. t`
`False := λt. λf. f`
`If l m n := λl. λm. λn. l m n`

`En Haskell:`

`true = \ifTrue ifFalse -> ifTrue`
`false = \ifTrue ifFalse -> ifFalse `
`iff = \condition ifTrue ifFalse -> condition ifTrue ifFalse`

`o, lo que es (casi) lo mismo:`

`true = const`
`false = flip const`
`iff = id`

`Luego, las siguientes consultas son validas:`

`> iff true 4 5`
`4`
`> iff false 'a' 'b'`
`'b'`

##### Definición de recursividad

##### Definición de los enteros

### Función en teoría de categorías

(heavy)

### Función como un TAD

Las funciones currificadas, en tanto valores, pueden ser también pensandas como un [TAD](tipo-abstracto-de-dato.html), para el cual:

-   su única operación primitiva es la aplicación, definida entre una función y otro valor. Esta operación, a su vez también es una función, llamada apply, (función ($) en el Prelude de Haskell)
-   sus valores son cada una de las funciones posibles. Así, por ejemplo, even, odd, (+) son todos valores del tipo función

### Funciones en Haskell

`Haskell es un lenguaje que implementa las nociones del paradigma funcional, y en particular, toma las gran mayoría de las nociones `
`* Las funciones`

Programa como combinación de funciones
--------------------------------------

c. Concepto de función en \*Haskell\*. Toda función es un valor, pero todo valor, es una función? Las alternativas son:

-   Sí. Todo es una función. De allí la evaluación diferida. Aun sí

tiene cero argumentos.

-   No. Función es solo aquello que tiene tipo (-&gt;) a b. La

evaluación diferida es propia de las expresiones, sean o no funciones. Función es aquello que es aplicable, es decir, que representa una transformación con dominio e imagen.

En ambos casos, valdría la pena revisar la consistencia de estas definiciones con el cálculo lambda. Y también, si damos a Haskell por implementación pura o no del mismo.

La función es un tad

`  2. Las funciones que no reciben parámetros ... ehhh quiero decir .... no existe tal cosa, una función es algo sensible de ser aplicado con parámetros`
`  3. Aquello que no es función es valor (no se si usaría el término constante, porque los valores en funcional son constantes no pueden cambiar)`
`  4. Aquello que puedo usar como argumento de una función o como valor de retorno de una función es un valor`
`  5. Existen funciones que pueden recibir funciones por parámetro o retornar una función`
`  6. Las funciones son valores`
