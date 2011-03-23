¿Qué entendemos por Binding?
----------------------------

El binding es la relación que se establece entre la invocación a un procedimiento-función-método y el código que se ejecuta. Otro nombre que le damos es *dispatch*.

Es algo central al paradigma de objetos, porque es lo que da pie al polimorfismo. Si se acuerdan de que cuando comenzamos con objetos diferenciamos el concepto de objeto y el de mensaje: bueno el binding es en definitiva el mecanismo que define qué método se ejecuta al enviar un mensaje. El method lookup es una de los mecanismos que llevan a cabo el binding pero no es la única.

Esencialmente, lo que nos interesa es entender en qué momento se produce ese binding, y hay básicamente dos opciones:

-   Al compilar = early binding o estático
-   Al ejecutar = late binding o dinámico

Ejemplos
--------

Tres ejemplos del lenguaje Java:

-   Al mandar un mensaje a dos objetos (polimórficos) distintos, el binding es dinámico.
-   Si un objeto recibe dos mensajes con el mismo nombre pero distintos tipos de parámetros, el bingind es estático. A esto lo llamamos sobrecarga.
-   Si invocamos un método de clase, el binding es estático.

Algunas variantes

-   En C++ o C\# si no les pongo virtual a los métodos el binding es estático.
-   En Eiffel se dá dinámicamente, a esta característica la denominamos multimethods.
-   En Smalltalk el binding de los métodos de clases es también dinámico. (Y esto es uno de los motivos que nos da pie a decir que las clases en Smalltalk son realmente objetos, mientras que en Java no es así.)

