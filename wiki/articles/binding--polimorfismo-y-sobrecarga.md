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

Polimorfismo de subtipos
------------------------

\\begin{frame}\[fragile\]{Polimorfismo de subtipos}

` \begin{itemize}`
`   \item Un objeto tiene muchos tipos.`
`   \item Por ejemplo las instancias de \code{ClrCellClass} tienen los tipos \code{ClrCellType} y \code{CellType}.`
`   \item \blue{Subsumption rule}: `
` \end{itemize}`
` \begin{center}`
`   $\begin{array}{c}`
`   \regla`
`     {\sequ{\Gamma}{M : S'} \quad S' <: S }`
`     {\sequ{\Gamma}{M : S}}{}`
`   \end{array}$`
` \end{center}`
` \begin{itemize}`
`   \item Eso me permite cosas como:`
`     \begin{lstlisting}[language=sool]`
`   c1:CellType := new ClrCellClass`
`     \end{lstlisting}`
`   \item La subsumption conlleva una pérdida de información:`
`     \begin{lstlisting}[language=sool]`
`   c1 <= setColor(green) // Imposible!!!`
`     \end{lstlisting}`
` \end{itemize}`

\\end{frame}

\\begin{frame}\[fragile\]{Problemas con subtipos en objetos}

` \begin{itemize}`
`   \item Cuadrados vs. rectángulos.`
```    \item Si entendemos herencia como \textbf{``es un ```*`},` `un` `cuadrado` ``` ``es ``` `un`*` rectángulo`
`   \item Por lo tanto cuadrado debería ser un subtipo de rectángulo.`
`   \item Sin embargo, los rectángulos podrían tener métodos que son inadecuados para un cuadrado.`
`     \begin{lstlisting}[language=sool,mathescape=true]`
` Rectangle = ObjectType { `
`   height: Void -> Integer`
`   width: Void -> Integer`
`   stretch: (Integer $\times$ Integer) -> Void // Problema`
` } `
`     \end{lstlisting}`
`   \item Este problema se magnifica si colapsamos clases y tipos.`
`   \item \textbf{Programa: Estudiar la relación entre subclases y subtipos.}`
` \end{itemize}`

\\end{frame}

\\subsection{Algunos problemas adicionales} \\frame{\\tableofcontents\[sectionstyle=show/shaded,subsectionstyle=show/shaded/shaded\]}

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

