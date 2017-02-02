---
layout: article
title: Binding  polimorfismo y sobrecarga
---

¿Qué entendemos por Binding?
----------------------------

El binding es la relación que se establece entre la invocación a un procedimiento-función-método y el código que se ejecuta. Otro nombre que le damos es *dispatch*. Los conceptos de binding, polimorfismo y sobrecarga son aplicables a muchos paradigmas, y tienen diferentes formas de ser entendidos en cada uno de ellos. Por el momento, este artículo se concentra fundamentalmente en el paradigma de objetos.

Dynamic method invocation
-------------------------

En objetos, cada vez que se envía un mensaje, el método invocado depende del receptor. Por ejemplo, en Smalltalk:

`figuras := {Cuadrado new. Rectangulo new. Circulo new}.`
`figuras collect: [:f | f superficie ]`

Si las clases Cuadrado, Rectangulo y Circulo tienen cada una su propia implementación de , cada vez que se envía el mensaje a la referencia el método invocado será uno distinto. Esta característica es una de las más importantes para el paradigma de objetos, ya que habilita algunas de sus herramientas más interesantes.

Tipo estático y tipo dinámico
-----------------------------

Al extender el concepto de dynamic method invocation a un lenguaje con chequeo estático de tipos, es necesario tener en cuenta la información estática que provee el sistema de tipos. En este tipo de lenguajes, a cada expresión se le asigna un tipo en forma estática. Estos tipos se ejecutan para validar la corrección de los programas antes de su ejecución y eliminar algunos posibles errores.

El valor que tome la expresión en tiempo de ejecución puede diferir del valor que es asignado a esa expresión en forma estática. Por ejemplo:

`public interface Figura {`
`    public double getSuperficie();`
`}`
`public class Circulo implements Figura {`
`    @Override`
`    public double getSuperficie() { ... }`
`    `
`    public double getRadio() { ... }`
`}`
`Figura f = new Circulo();`

Estáticamente, la variable tiene el tipo , pero cuando ese programa se ejecute, el valor de será un Circulo. La asignación `code|Figura` `f` `=` `new` `Circulo()` es válida siempre que sea un **subtipo** de . A esta regla se la conoce como **subsumption**.

La presencia de tipado estático nos obliga a analizar nuestro código desde las dos perspectivas. Al evaluar se envía el mensaje al objeto *referenciado* por la variable y por lo tanto se ejecutará el método de la clase . Sin embargo, en el momento de chequear tipos se controla que el tipo de , que es contenga el mensaje enviado.

Si intentamos evaluar se produciría un error de compilación, porque esa expresión no pasa el chequeo de tipos. Por eso, decimos que la regla de subsumption conlleva una pérdida de información de tipos. El objeto referenciado por entiende el mensaje , sin embargo el chequeador de tipos no puede verificar eso y rechaza el programa.

Sobrecarga
----------

Se dice que un *nombre de método* está **sobrecargado** en un contexto cuando es utilizado para representar dos o más métodos o funciones distintos, que se diferencian por su tipo. El tipo de un método incluye tanto a los tipos de los parámetros como al del valor de retorno. Por ejemplo:

-   En Haskell es posible definir funciones con el mismo nombre y distintos tipos de parámetros o distinto tipo de retorno.
-   En Java o C\# es posible definir dos métodos en la misma clase o jerarquía con distinto número de parámetros o con parámetros de distinto tipo. (La posibilidad de sobrecargar métodos variando el tipo de retorno es menos frecuente en el paradigma de objetos.)

Por ejemplo el método está sobrecargado en la clase .

`class C {`
`    void m(Figura f) { println(1); }`
`    void m(Circulo c) { println(2); }`
`}`

Cuando se envía un mensaje que está sobrecargado, el sistema debe decidir cuál es el método que se debe ejecutar. En la mayoría de los lenguajes orientados a objetos, esta decisión se toma en forma estática. (Cuando la decisión es dinámica, en lugar de sobrecarga hablamos de [Multimethods](-multimethods.html). Por ejemplo, en la siguiente porción de código, al evaluar se ejecutará el método que recibe una figura, y no el que recibe un círculo; y por lo tanto imprimirá "1".

`Figura f = new Circulo();`
`C c = new C();`
`c.m(f); // => imprime 1!`

La interpretación que debemos hacer es que que en realidad el mensaje enviado no se identifica únicamente por su nombre, sino que incluye los tipos de los parámetros. Desde esta perspectiva los dos métodos de la clase tienen distinto nombre, son totalmente independientes uno del otro. Debemos interpretar que el primero se denomina y el segundo . En presencia de este tipo de sobrecarga el *método a ejecutar* se decidirá en tiempo de ejecución, en función del mensaje enviado, pero el *mensaje a enviar* se decide en tiempo de compilación, a partir de la información de tipos disponible en este momento. En resumen, el mensaje enviado a no es sino . Dado que ambos métodos tienen identificadores distintos, para invocarlos se envían mensajes distintos y la decisión entre ambos será tomada en tiempo de compilación.

Veamos un ejemplo más complejo con sobrecarga y redefinición (adaptado de ["Foundations of Object-oriented Languages: Types and Semantics" por Kim Bruce](http://books.google.com.ar/books?id=9NGWq3K1RwUC&pg=PA27&dq=Kim+Bruce+overloading+vs+overriding&hl=es-419&sa=X&ei=I65AUuyWL4Om9gT5ooCQDw&ved=0CDEQ6AEwAA#v=onepage&q=Kim%20Bruce%20overloading%20vs%20overriding&f=false)), tenemos el siguiente código en Scala:

`class C {`
`   def m(other: C) = { println(1) }`
`}`
`class SC extends C{`
`   def m(other: SC) = { println(3) }`
`   override def m(other: C) = { println(2) }`
`}`

Si hacemos la siguiente prueba:

`var c2: C = new SC()`
`var sc: SC = new SC()`
`c2.m(sc)`

El resultado será 2, no 3, ya que el mensaje que se elige estáticamente no es m(SC), sino m(C). Para entender lo que sucede podemos hacer dos pasos bien separados: Pensar primero como compilador y después pensar como máquina virtual (pensar en tiempo de compilación y tiempo de ejecución).

Empecemos como compiladores: c2 es una variable del tipo C, y sc es una variable del tipo SC. En c2 se referenciará a una instancia del tipo SC, pero eso como compiladores por ahora no nos importa, porque no estamos ejecutando el código.

Luego vamos a esta llamada:

Acá es donde empieza la confusión: Uno piensa que porque sc es del tipo SC y c2 es una instancia de SC (si bien la variable es de tipo C) el método que se va a ejecutar es m(SC), pero no es así. ¿Por qué? La respuesta viene de pensar como compilador, no "ejecutar" el código y mirar la el tipo de la variable c2, y deducir de ahí los posibles mensajes que entiende, y de ver bien la firma de esos mensajes.

¿De qué tipo es C2? Del tipo C. ¿Qué mensajes entiende un objeto del tipo C? Vamos a ver la declaración de C:

`class C {`
`   def m(other: C) = { println(1) }`
`}`

Los objetos del tipo C sólo entienden un mensaje, y ese es m(C). Por lo tanto, siendo que SC es un subtipo de C, la firma del mensaje que va a ser llamado en va a ser m(C):Unit y no m(SC):Unit, dado que el tipo C no tiene definido ningún mensaje con parámetro SC. Esa firma es decidida en compilación y depende sólo de los tipos declarados/inferidos de las variables, y no de las instancias.

Ahora sabemos la firma, terminamos de compilar el código. Pasemos al modo ejecución y pensemos como máquinas virtuales ¿Qué método se ejecuta al hacer c2.m(sc)? Para saberlo, no hay que hacer otra cosa que method lookup.

1. ¿Cuál es la firma del mensaje c2.m(sc)? La firma es m(C):Unit (por lo que vimos antes en "compilación".

2. ¿A quién le estoy enviando ese mensaje? Al objeto referenciado por c2, que es una instancia de la clase SC.

3. Para saber qué método ejecutará entonces, comenzamos el method lookup a partir de la clase SC.

`class SC extends C {`
`  def m(other: SC) = { println(3) }`
`   `
`  override def m(other: C) = { println(2) }`
`}`

4. ¿Tiene SC algún método definido cuya firma sea m(C):Unit? Sí. Entonces ese es el método que se va a ejecutar, que es el que imprime 2.

Eso sería básicamente todo. Lo importante de todo esto es entender que ahora los tipos son mucho más importantes para la construcción de mi modelo conceptual. Y la idea es que con ejercicios como estos uno comprueba si entendió o no que los tipos me cambian un poquito la semántica del código y me agregan algo más en lo que tengo que pensar, si bien me previenen algunos errores en compilación.

Ejemplos
--------

Tres ejemplos del lenguaje Java:

-   Al mandar un mensaje a dos objetos (polimórficos) distintos, el binding es dinámico.
-   Si un objeto recibe dos mensajes con el mismo nombre pero distintos tipos de parámetros, el bingind es estático. A esto lo llamamos sobrecarga.
-   Si invocamos un método de clase, el binding es estático.

Algunas variantes

-   En C++ o C\# si no les pongo virtual a los métodos el binding es estático.
-   En Eiffel se da dinámicamente, a esta característica la denominamos multimethods.
-   En Smalltalk el binding de los métodos de clases es también dinámico. (Y esto es uno de los motivos que nos da pie a decir que las clases en Smalltalk son realmente objetos, mientras que en Java no es así.)

