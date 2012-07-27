Introducción
------------

### ¿Qué es un tipo?

Toda expresión en un programa puede denotar diferentes valores en diferentes momentos (dependiendo del lenguaje eso puede ocurrir en la misma o en diferentes ejecuciones del programa). Un *tipo* describe un conjunto de valores. Al asignarle un tipo a una expresión se está delimitando cuál es el conjunto de valores que podría denotar esa expresión.

Por otro lado, las operaciones computacionales en general no pueden ser aplicadas sobre cualquier valor. Un sistema de tipos me provee una forma de estudiar qué valores tienen sentido para ser utilizados en una operación dada. Por ejemplo, si x e y son valores numéricos, la expresión x + y tiene sentido. En cambio si x e y son valores booleanos, x + y carece de sentido.

La idea de tipo nos permite relacionar:

-   un conjunto de valores que *tienen* ese tipo o *son* de ese tipo,
-   con las operaciones que pueden ser realizadas sobre esos valores.

### Sistemas de tipos

Existen diferentes tendencias en cuanto a cómo se clasifican a los diferentes lenguajes en función de la presencia o no de un sistema de tipos o de las características de ese sistema de tipos. Diferentes autores hablan de lenguajes tipados o no tipados, fuerte o débilmente tipados, lenguajes estáticos o dinámicos. Intentaremos proveer una forma de categorización que sea amplia para poder estudiar diferentes lenguajes y autores.

Según Luca Cardelli un *lenguaje tipado* es el que asocia cada expresión con un tipo (con uno no trivial). El *sistema de tipos* es el componente que administra la información de tipos de un programa. Un lenguaje se considera tipado por la existencia de un sistema de tipos, independientemente de que la sintaxis del lenguaje incorpore información de tipos.

Los objetivos de un sistema de tipos son:

-   Ayudar a detectar errores al programar.
-   Guiar al programador sobre las operaciones válidas en un determinado contexto, tanto en cuanto a documentación com en cuanto a ayudas automáticas que puede proveer por ejemplo un IDE.
-   En algunos casos el comportamiento de una operación puede variar en función del tipo de los elementos involucrados en la misma. De esto vamos a diferenciar varios sabores: polimorfismo, sobrecarga, multimethods, etc.

Algunas características de un sistema de tipos:

-   Proveen información al programador, de forma más precisa que un comentario.
-   Según Cardelli, un sistema de tipos debería permitir hacer validaciones utilizando la información de tipos, algorítmicamente.
-   Las validaciones basadas en un sistema de tipos, que son más fácilmente automatizables que otros tipos de especificaciones formales.

Chequeos estáticos y dinámicos
------------------------------

Algunos ejemplos de cómo se comportan los tipos en Java y en Smalltalk

-   En Java tomamos un String e intentamos mandarle el mensaje "caminar", no compila: chequeo estático.
-   En Smalltalk hago lo mismo y me permite ejecutarlo, se rompe en tiempo de ejecución: chequeo dinámico o en tiempo de ejecución.

Eso nos marca una primera categorización de los tipos: en función de en qué momento se chequean. No necesariamente un lenguaje tiene siempre el mismo comportamiento, en java si yo casteo postergo el chequeo hasta el tiempo de ejecución. Si se acuerdan de lenguaje C, recordamos que ante un casteo puede pasar cualquier cosas: a veces simplemente no se valida nada.

Esta primera diferencia parecería volcar la balanza a favor de los lenguajes con chequeo estático, porque detectan antes los problemas, sin embargo vamos a ver que también tienen desventajas. Por otro lado, ante la presencia de casteos el chequeo estático se pierde.

Finalmente es importante diferenciar entre chequeo dinámico y no chequeo. El chequeo dinámico me permite manejar el problema y manejarlo o por lo menos me lo informa correctamente, la ausencia de chequeo suele causar errores muy difíciles de detectar o corregir.

### Detección de errores

Podemos discriminar dos tipos de errores que se pueden producir durante la ejecución de un programa:

Trapped (atrapados)  
Son los errores que se detectan inmediatamente, por ejemplo una división por cero.

Untrapped (no atrapados)  
Son errores que pueden no ser detectados. El programa podría continuar ejecutándose por un tiempo antes de detectar el problema. Esto puede ocurrir en algunos lenguajes por ejemplo si se accede a posiciones de un array más allá de su longitud o se salta a una posición inválida de memoria.

Un programa se considera **seguro** (safe) si no causa untrapped errors. Un lenguaje se considera seguro si todo programa escrito en ese lenguaje está excento de untrapped errors.

En todo lenguaje se puede designar un conjunto de los errores como *prohibidos*. Los errores prohibidos deberían incluir a todos los errores *untrapped*, más un subconjunto de los errores *trapped*.

Tipos nominales y estructurales
-------------------------------

Eso nos lleva a una clasificación más: tipos estructurales vs. tipos nominales. Llamamos nominales a los tipos que tienen un nombre. En Java los tipos son nominales están dados por las clases (concretas o abstractas) y por las interfaces. En Smalltalk los tipos son estructurales, por ejemplo todos los objetos que entienden \#value son un tipo. La nominalidad del Java se ve en que si hago dos interfaces con los mismos mensajes, los objetos que implementan la primera no son polimórficos con los objetos que implementan la segunda.

Algunas combinaciones posibles
------------------------------

Suele haber una fuerte relación entre los tipos nominales-explícitos y estructurales-implícitos. Esto es porque uno para explicitarlos le pone nombre, pero en algunos lenguajes como Scala existen tipos estructurales y explícitos.

También suele haber una fuerte relación entre el chequeo estático y el tipado explícito o nominal, sin embargo en Haskell podemos ver que tenemos tipos implícitos y chequeo estático. Normalmente esto va asociado a un concepto llamado "inferencia de tipos", es decir, ¿cómo se hace para poder chequear el tipo de algo sin que me lo digas? Tiene que poder inferirlo el compilador por sí mismo.

Y acá se ven las desventajas de los sistemas de tipado más "fuertes":

-   Obligan a la burocracia de decir de qué tipo va a ser cada cosa y de indicar qué cosas pueden ser polimórficas.
-   Muchas veces me restringen las posibilidades que tengo al programar.

Gran parte del trabajo en sistemas de tipos que se hace intenta construir sistemas o lenguajes que puedan evitar errores minimizando la burocracia y/o eliminando las restricciones innecesarias.

Clasificaciones
---------------

En resumen planteamos tres clasificaciones:

1.  En cuanto a la forma de chequeo puede ser estático/compile time, dinámico/runtime o nada.
2.  En cuanto a la forma de especificar el tipo de algo puede ser explícito o implícito / inferido.
3.  En cuanto a la forma de constituir un tipo puede ser nominal o estructural (no sé si agregar dependientes, también hay más variantes).

Más complicada es la clasificación en cuanto a los tipos de polimorfismo que se banca (en particular en los lenguajes con tipado estático).

Algunos ejemplos
----------------

-   Java tiene un tipado estático, nominal y explícito (en presencia de casteos se vuelve dinámico)
-   Smalltalk es dinámico, estructural e implícito.
-   Self lo mismo.
-   Haskell es estático pero se banca ser explícito o implícito (inferencia de tipos) y en algunos casos también se comporta estructuralmente.
-   Scala es estático, tiene algo de inferencia y también soporta tipos estructurales y nominales.
-   ObjectiveC puede ser estático o dinámico si usas los ids.
-   C aparenta ser estático, pero ante casteos yo veo que el tipado es nulo.

