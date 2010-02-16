Dado que el haskell es un lenguaje con [Inferencia de Tipos](inferencia-de-tipos.html), no es necesario indicar el tipo de las funciones que construimos. A pesar de ello (o talvez precisamente *por* ello) los tipos juegan un rol fundamental al programar en un lenguaje funcional (en particular en Haskell).

Esto se debe a varios motivos:

El sistema de tipos  
El sistema de tipos de Haskell es muy estricto, bastante más de lo que estamos acostumbrados los que venimos de la programación orientada a objetos. Eso muchas veces puede ser bueno porque nos ayuda a detectar errores más tempranamente (por ejemplo no es posible hacer casteos en Haskell).

En otros casos puede ser molesto porque el tipado tan estricto complica algunas operaciones sencillas.

Ambas situaciones obligan al programador a ser más atento con los tipos de cada una de las funciones que programa o que utiliza.

<!-- -->

La construcción de funciones a partir de funciones  
El paradigma funcional tiene como uno de sus pilares la posibilidad de construir funciones complejas a partir de combinar funciones más simples (utilizando composición, aplicación parcial, orden superior, etc); para poder utilizar cualquiera de esas herramientas es necesario tener presente el tipo de las funciones que quiero combinar, por ejemplo:

-   Si quiero componer dos funciones: es necesario que la imagen de f esté incluida en el dominio de g (tal como aprendimos en matemática).
-   Al aplicar una función de orden superior es necesario que matcheen los tipos de ambas funciones.

<!-- -->

El polimorfismo paramétrico y las type classes  
Una función que tiene un tipo genérico al ser aplicada puede reducir su tipo, eso también es algo a tener en cuenta. Por ejemplo la función puede en principio procesar cualquier tipo; en cambio una vez que yo le aplico el primer parámetro (el criterio de selección) ese tipo se restringe, la función sólo va a servir para listas de valores numéricos: combinar filter con even restringe el tipo de ambas.

A continuación se describen paso a paso los ejemplos que permiten comprender el mecanismo de inferencia utilizado en el lenguaje Haskell.

Antes de empezar: cómo leer el tipo de una función en Haskell
-------------------------------------------------------------

Aplicación
----------

si x es Bool, entonces {{code|not x} también es de tipo Bool

Funciones Simples
-----------------

Al intentar calcular el tipo de una función, lo primero que tenemos que hacer es mirar las funciones que se usan dentro de su definición. Por ejemplo:

`none x y = not x && not y`

Para determinar el tipo de la función none podemos seguir los siguientes pasos:

1.  Sabemos que tiene dos parámetros (x e y), entonces podemos decir que su tipo tiene que tener la forma , luego deberemos calcular cuáles son a, b y c.
2.  Si x es utilizado como parámetro de la función , entonces su tipo deberá ser compatible con el de not. El tipo de not es: ; es decir: recibe un booleano y devuelve otro booleano. Por lo tanto x deberá ser un booleano.
3.  Un razonamiento análogo nos lleva a deducir que y también debe ser un valor booleano. Entonces sabemos que el tipo de la función tiene la forma , es decir: recibe dos Booleanos y nos falta saber qué devuelve.
4.  Finalmente, para saber el tipo de retorno podemos mirar que si x es Bool, entonces también es de tipo Bool, al igual que . Por otro lado la función tiene el tipo (recibe dos booleanos y devuelve también un booleano), entonces el valor de retorno también es un booleano.

En el último paso podemos ver que en realidad para saber el tipo de no sería necesario mirar los parámetros de , con saber su tipo de retorno sería suficiente. Sin embargo el análisis es útil para asegurarnos de que la función es correcta.

Por otro lado, en ejemplos más complejos analizar los parámetros será indispensable para poder saber el tipo de retorno (por ejemplo en la presencia de polimorfismo).
