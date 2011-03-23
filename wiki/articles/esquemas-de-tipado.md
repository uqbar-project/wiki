¿Qué es un tipo?

-   conjunto de valores o "elementos de ese conjunto"
-   conjunto de operaciones que puedo realizar sobre ellos

Los objetivos de un sistema de tipos son:

-   En primer lugar ayudar a detectar errores al programar.
-   Guiar al programador sobre las operaciones válidas en un determinado contexto, tanto en cuanto a documentación com en cuanto a ayudas automáticas que puede proveer por ejemplo un IDE.
-   En algunos casos el comportamiento de una operación puede variar en función del tipo de los elementos involucrados en la misma. De esto vamos a diferenciar varios sabores: polimorfismo, sobrecarga, multimethods, etc.

A veces se habla de lenguajes tipados o no tipados, fuerte o débilmente tipados, lenguajes estáticos o dinámicos. Esas pueden ser ideas intuitivas que ayudan, pero en este momento estamos en condiciones de ser más estrictos en nuestra clasificación.

Chequeos estáticos y dinámicos
------------------------------

Algunos ejemplos de cómo se comportan los tipos en Java y en Smalltalk

-   En Java tomamos un String e intentamos mandarle el mensaje "caminar", no compila: chequeo estático.
-   En Smalltalk hago lo mismo y me permite ejecutarlo, se rompe en tiempo de ejecución: chequeo dinámico o en tiempo de ejecución.

Eso nos marca una primera categorización de los tipos: en función de en qué momento se chequean. No necesariamente un lenguaje tiene siempre el mismo comportamiento, en java si yo casteo postergo el chequeo hasta el tiempo de ejecución. Si se acuerdan de lenguaje C, recordamos que ante un casteo puede pasar cualquier cosas: a veces simplemente no se valida nada.

Esta primera diferencia parecería volcar la balanza a favor de los lenguajes con chequeo estático, porque detectan antes los problemas, sin embargo vamos a ver que también tienen desventajas. Por otro lado, ante la presencia de casteos el chequeo estático se pierde.

Finalmente es importante diferenciar entre chequeo dinámico y no chequeo. El chequeo dinámico me permite manejar el problema y manejarlo o por lo menos me lo informa correctamente, la ausencia de chequeo suele causar errores muy difíciles de detectar o corregir.

Tipado implícito o explícito
----------------------------

El tipado de Java es explícito, esto se ve en dos aspectos:

-   Toda variable, parámetro, método tiene un tipo definido.
-   Para que dos objetos puedan ser polimórficos tengo que indicarlo explícitamente: herencia o interfaces.

Las interfaces cumplen dos funciones:

-   Permitir un polimorfismo chequeado estáticamente y no restringido por la herencia simple.
-   Ponerle nombre a un concepto que no quiero representar con una clase: documentar.

En Smalltalk no necesito hacer una indicación explícita de que dos objetos sean polimórficos, basta con que entiendan algún(os) mensaje(s) en común. Vemos el ejemplo de los bloques y los símbolos.

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

