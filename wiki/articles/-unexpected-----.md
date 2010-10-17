Un error que puede aparecer al programar en haskell es:

`ERROR `[`file:.\tp1`](file:.\tp1)` funcional:6 - Syntax error in expression (unexpected`
`` `;', possibly due to bad layout) ``

¡Pero yo no puse ningún punto y coma! ¿Qué onda?

La respuesta corta es que hay que revisar la sintaxis y en particular la *indentación*. El por qué de ese mensaje tan raro, a continuación.

Indentación
-----------

El haskell usa una sintaxis "bidimensional" (no se asusten), eso quiere decir que si vos "indentás" algo, eso tiene un significado. Entonces antes de continuar hay que entender el concepto de indentación. Indentar algo es dejar espacios o tabs adelante del código, generalmente uno lo hace para que se lea mejor, por ejemplo

function fact(n: integer): longint; begin

`   if (n = 0) then`
`       fact := 1`
`   else`
`       fact := n * fact(n - 1);`

end;

Podemos ver que el programador dejó algunos espacios antes de las palabras if y then, entonces visualmente es claro que todo eso queda dentro del bloque definido por begin/end. De la misma manera las líneas de código que están dentro del if y del else tienen aún más espacios ("más indentación").

Sintaxis "bidimensional"
------------------------

Como dijimos, el haskell utiliza una sintaxis bidimensional, es decir, en pascal o c la indentación son opcionales, no tienen significado, es decir, uno los pone sólo para entender mejor el programa; en cambio en haskell no hacen falta los / de pascal ni las llaves de C para demarcar un bloque de código. En haskell un bloque es demarcado directamente con la indentación.

Entonces la función que en C se escribiría:

`int suma(int a, int b)`
`{`
`   return a + b;`
`}`

En haskell no necesita de las llaves, porque se da cuenta a partir de la indentación:

`suma a b =`
`   a + b`

Tampoco son necesarios los tipos ni el return, pero eso es otro tema, lo que nos interesa acá es que no son necesarias las llaves ni el punto y coma, porque con la indentación y el fin de línea es suficiente para que el compilador entienda dónde empieza y termina la función. (En este caso sería aún más simple poner todo en una sola línea, pero algunas funciones complejas tienen más de una línea.)

En resumen, si vos indentás mal es análogo a olvidarse una llave o un o un ';' en otro lenguaje.

¿Por qué entonces dice *unexpected ';*', si yo no puse ningún ';'?
------------------------------------------------------------------

Se puede entender como que el haskell a partir de la sintaxis bidimensional "completa" los demarcadores de comando y de bloque, de una forma similar al C, quedando algo parecido a:

`suma a b = {`
`   a + b;`
`}`

De hecho, es posible escribir la función de esa manera si uno prefiere... pasa que obviamente uno prefiere la sintaxis con menos chirimbolos.

Entonces, cuando dice "unexpected ';'" lo más probable es que no está entendiendo el "layout" que ustedes le dieron a al función, es decir, la forma en que la indentaron. Ese "bad layout" hace que en su traducción aparezca un ";" en algún lado que no tiene sentido. Saber dónde está ese ";" no es sencillo, hay que revisar el layout de la función.
