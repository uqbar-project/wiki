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

Se dice que un *nombre de método* está **sobrecargado** en un contexto cuando es utilizado para representar dos o más métodos *distintos*, que se diferencian por su tipo o *firma*. Por ejemplo

-   Dos funciones con el mismo nombre y distinto número de parámetros o parámetros de distinto tipo.
-   Dos métodos definidos para el mismo objeto con distinto número de parámetros o parámetros de distinto tipo.

Por ejemplo el método está sobrecargado en la clase .

`class C {`
`    void m(Figura f) { println(1); }`
`    void m(Circulo c) { println(2); }`
`}`

Cuando se envía un mensaje que está sobrecargado, el sistema debe decidir cuál es el método que se debe ejecutar. En la mayoría de los lenguajes orientados a objetos, esta decisión se toma en forma estática. Por ejemplo, en la siguiente porción de código, al evaluar se ejecutará el método que recibe una figura, y no el que recibe un círculo; y por lo tanto imprimirá "1".

`Figura f = new Circulo();`
`C c = new C();`
`c.m(f);`

`C c1 := new C();`
`C c2 := new SC();`
`C sc := new SC();`

` \begin{lstlisting}[language=sool]`
` class C {`
`   function equals(other:CType) : Boolean is { writeln (1) }`
` }`

` class SC {`
`   function equals(other:CType) : Boolean is { writeln (2) }`
`   function equals(other:SCType) : Boolean is { writeln (3) }`
` }`

` CType = ObjectType { equals: CType -> Boolean }`
` SCType = ObjectType { `
`   equals: CType -> Boolean;`
`   equals: SCType -> Boolean `
` }`
` \end{lstlisting}`

\\end{frame}

\\begin{frame}\[fragile\]{Sobrecarga - Ejemplo}

` ¿Qué método se invoca en cada caso?`
` \begin{lstlisting}[language=sool]`
`   c1: CType := new C;`
`   c2: CType := new SC;`
`   sc: SCType := new SC;`

`   c1 <= equals(c1);`
`   c1 <= equals(c2);`
`   c1 <= equals(sc);`

`   c2 <= equals(c1);`
`   c2 <= equals(c2);`
`   c2 <= equals(sc);`
`   `
`   sc <= equals(c1);`
`   sc <= equals(c2);`
`   sc <= equals(sc);`
` \end{lstlisting}`

\\end{frame}

\\begin{frame}\[fragile\]{Multimethods}

` \begin{itemize}`
`   \item El método a ejecutar depende de los valores de uno o más de los parámetros del método.`
`   \item \cite{bruce2002} los presenta con una sintaxis procedural:`
`   \begin{lstlisting}[language=sool]`
` function equal(p1:Point, p2:Point): Boolean is { ... }`
` function equal(p1:ColorPoint, p2:ColorPoint): Boolean is { ... }`
`   \end{lstlisting}`
`   \item Otros lenguajes mantienen la sintaxis que destaca a uno de los parámetros como \textit{receptor}.`
`   \item Suele confundirse con la sobrecarga.`
` \end{itemize}`

\\end{frame}

\\section{Varianza} \\frame{\\tableofcontents\[sectionstyle=show/shaded,subsectionstyle=show/shaded/shaded\]}

Es algo central al paradigma de objetos, porque es lo que da pie al polimorfismo. Si se acuerdan de que cuando comenzamos con objetos diferenciamos el concepto de objeto y el de mensaje: bueno el binding es en definitiva el mecanismo que define qué método se ejecuta al enviar un mensaje. El method lookup es una de los mecanismos que llevan a cabo el binding pero no es la única.

Esencialmente, lo que nos interesa es entender en qué momento se produce ese binding, y hay básicamente dos opciones:

-   Al compilar = early binding o estático
-   Al ejecutar = late binding o dinámico

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

