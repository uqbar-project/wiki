¿Qué entendemos por Binding?
----------------------------

El binding es la relación que se establece entre la invocación a un procedimiento-función-método y el código que se ejecuta. Otro nombre que le damos es *dispatch*. Los conceptos de binding, polimorfismo y sobrecarga son aplicables a muchos paradigmas, y tienen diferentes formas de ser entendidos en cada uno de ellos. Por el momento, este artículo se concentra fundamentalmente en el paradigma de objetos.

Dynamic dispatch
----------------

En objetos, cada vez que se envía un mensaje, el método invocado depende del receptor. Por ejemplo, en Smalltalk:

`figuras := {Cuadrado new. Rectangulo new. Circulo new}.`
`figuras collect: [:f | f superficie ]`

Cada vez que se envíe el mensaje a la referencia .

\\begin{frame}\[fragile\]{Dynamic Method Invocation}

` \begin{itemize}`
`   \item El método invocado depende del \blue{receptor}.`
`   \item Ejemplo`
`     \begin{lstlisting}[language=sool]`
` c1:CellType := new ClrCellClass`
` c1 <= bump() // Ejecuta el bump de CellClass`

` class CellClass { ...`
`   function bump(): Void is { `
`     self <= set(...) // Depende del receptor`
`   }`
` }`

` class ClrCellClass ... `
`   function set(nuVal: Integer): Void is`
`     { super <= set(nuVal); self.color := red }`
` }`
` \end{lstlisting}`
`   \item Incluye los mensajes a \code{self}`
`   \item Dentro de \code{set}, la variable \code{self} tiene tipo \code{ClrCellType}.`
` \end{itemize}`

\\end{frame}

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

\\begin{frame}\[fragile\]{Dynamic Method Invocation}

` \begin{itemize}`
`   \item El método invocado depende del \blue{receptor}.`
`   \item Ejemplo`
`     \begin{lstlisting}[language=sool]`
` c1:CellType := new ClrCellClass`
` c1 <= bump() // Ejecuta el bump de CellClass`

` class CellClass { ...`
`   function bump(): Void is { `
`     self <= set(...) // Depende del receptor`
`   }`
` }`

` class ClrCellClass ... `
`   function set(nuVal: Integer): Void is`
`     { super <= set(nuVal); self.color := red }`
` }`
` \end{lstlisting}`
`   \item Incluye los mensajes a \code{self}`
`   \item Dentro de \code{set}, la variable \code{self} tiene tipo \code{ClrCellType}.`
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

\\begin{frame}\[fragile\]{Cast}

` \begin{itemize}`
`   \item La regla de subsumption conlleva una pérdida de información.`
`   \item Algunos lenguajes permiten agregar información en forma manual mediante un \blue{cast}`
`     \begin{lstlisting}[language=sool]`
`   c2 := (ClrCellClass) c1;`
`   c2 <= setColor(green) `
`     \end{lstlisting}`
`   \item Se posterga el chequeo de tipos al tiempo de ejecución.`
`   \item Es obviamente algo no deseado.`
`   \item No se debe confundir con una coerción.`
`     \begin{itemize}`
`       \item Java tiene coerciones sólo para los tipos básicos`
`       \item Utiliza la misma sintaxis para ambas cosas.`
`     \end{itemize}`

` \end{itemize}`

\\end{frame}

\\begin{frame}\[fragile\]{Sobrecarga}

` Un nombre de método está sobrecargado en un contexto si:`
`     \begin{itemize}`
`       \item Es utilizado para representar dos o más métodos \emph{distintos}.`
`       \item El método representado es determinado por el tipo o \blue{firma} (\emph{signature}) del método.`
`     \end{itemize}`
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

