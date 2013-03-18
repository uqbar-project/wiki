Como su nombre lo indica, la función es el concepto fundamental del paradigma funcional: mientras que, por ejemplo, en el paradigma de objetos, los problemas se resuelven en términos de envío de mensajes entre objetos, en funcional los programas se estructurarán en torno a aplicación de funciones sobre valores. La función, es así, pues, la [computación](computacion.html) característica del paradigma.

Enfoques
--------

### Función como caja negra

Una forma simple de pensar una función es como una máquina con una salida y al menos una entrada, capaz de producir un resultado. Decimos que se trata de una caja negra, porque para aquel que la use no tiene acceso al interior de la misma, sino tan solo a sus entradas y salida. Esto nos lleva a que las funciones pueden ser combinadas fácilmente, de diversas formas, tan solo conociendo el tipo de entradas y salidas que posee.

### Función como transformación matemática

Las funciones, sin embargo, no son simples cajas negras, que podrían, por ejemplo, tener memoria de sus entradas anteriores, sino que son transformaciones matemáticas que presentan [transparencia referencial](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html).

En particular, las funciones son relaciones que presentan las siguientes características:

-   para toda entrada aceptable (su dominio), existe un único resultado (imagen), lo cual se conoce como unicidad.
-   para toda entrada del dominio, existe una un sólo resultado, lo que se conoce como existencia.

### Función desde un punto de vista imperativo

Al llevar los conceptos de función matemática al mundo computacional, la transparencia referencial implica que las funciones, comparadas contra los procedimientos imperativos, no tienen [efecto](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html), su aplicación no afecta al contexto, o, cuando menos, no es visible para el observador que evalua la expresión.

Si bien no tiene sentido hablar de mutabilidad en el contexto matemático, dado que solo se manejan valores y no referencias (al menos, en un enfoque simplista), la transparencia referencial en los programas construidos en el paradigma funcional tiene dos consecuencias mas o menos evidentes:

-   Las funciones no pueden mutar sus argumentos ni otras variables, locales o globales, ni directa ni indirectamente. Esto se garantiza al eliminar la [asignación destructiva](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html) del lenguaje. (podemos decir que las variables no varían)
-   Las funciones no pueden realizar de forma directa operaciones de entrada/salida (aunque existen estrategias para realizarlas indirectamente preservando a la función pura)

### Función como un TAD

Las funciones currificadas, en tanto valores, pueden ser también pensandas como un [1](http://es.wikipedia.org/wiki/Tipo_abstracto_de_dato%7CTAD), para el cual:

-   su única operación primitiva es la aplicación, definida entre una función y otro valor. Esta operación, a su vez también es una función, llamada apply, (función ($) en el Prelude de Haskell). Las demás operaciones complementarias, como la composición, se construyen a partir de la aplicación.
-   sus valores son cada una de las funciones posibles. Así, por ejemplo, even, odd, (+) son todos valores del tipo función

### Función desde el cálculo lambda

Desde el punto de vista del [lambda](http://es.wikipedia.org/wiki/C%C3%A1lculo_lambda%7Ccálculo), la función es una primitiva del lenguaje, y todas las funciones son anónimas, es decir, son expresiones lambda. Por lo que la función `siguiente`, que en Haskell normalmente escribiríamos:

`siguiente` `x` `=` `x` `+` `1`

En cálculo lambda no tipado la escribiríamos:

`siguiente` `=` `𝛌x.sum` `x` `one`

Equivalent a la siguiente definición Haskell:

`siguiente` `=` `\x` `->` `x` `+` `1`

### Funciones en Haskell

Las funciones en Haskell presentan todas las carecterísticas mencionadas anteriormente. A modo de resumen, decimos que:

-   Las funciones son transformaciones matemáticas, que presentan transparencia referencial, y por tanto libres de efecto
-   Las funciones son valores
-   Las funciones tienen tipo función ((-&gt;) a b), que está determinado por su dominio e imagen. Una función de enteros en booleanos tiene tipo `Int` `->` `Bool`
-   Las funciones son un caso particular de las relaciones, que presentan unicidad y existencia para todo su dominio.
-   Las funciones están currificadas, por lo que no existen funciones de más de un argumento realmente, sino que se emulan a partir de funciones de un argumento que devuelven otra función que toma los parámetros restantes.
-   La operación primitiva de la función es la aplicación, por la cual se evalúa una función pasandole sus argumentos y obteniendo un resultado
-   El mecanismo de la evaluación de las funciones es la reducción (reducción 𝛃)
-   Dado que la única operación primitiva del tipo función es la aplicación, sólo es función aquello todo y sólo lo que pueda ser aplicado. Moraleja: no tiene sentido hablar de funciones de cero argumentos, ya que no pueden ser aplicadas
-   La [estrategia de reducción](estrategias-de-evaluacion.html) empleada por Haskell es no estricta (≅ call-by-name), lo cual no es lo mismo que evaluación diferida, que significa que las expresiones son evaluadas a medida que son necesarias. Sin embargo, en la práctica, una lleva normalmente a la otra, por lo que podremos decir sin excesivo rigor que Haskell presenta evaluación diferida.

