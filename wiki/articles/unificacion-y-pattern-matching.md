Unificación y Pattern-Matching
------------------------------

Dentro de los conceptos del paradigma lógico no está incluido el concepto de **asignación**. Para dejar en claro esto vamos a llamar a esta idea **Asignación Destructiva**, esto se debe a que una asignación me permite "destruir" el valor que tiene una variable en un momento dado y reemplazarlo por otro. Esta idea de asignación no tiene sentido cuando pensamos en incógnitas (si en un momento dado decimos que una variable X vale 1, su valor será 1 y ningún otro hasta que se empiece a buscar otra solución a la misma consulta, en ese momento se desligan todas las variables y se empieza de nuevo). Las variables en el paradigma lógico se asemejan a la idea de variable matemática, y el mecanismo por el cual se le dan valores a las variables se llama **unificación**. Cuando una variable que no tiene ningún valor pasa a tenerlo vamos a decir que dicha variable a sido **ligada**, en caso contrario la variable se encuentra **sin ligar o no ligada**.

Si nuestra base de conocimiento es

<code>padre(homero,bart).

padre(homero,maggie).

padre(homero,lisa).</code>

Y hago la consulta

`?-` `padre(X,lisa).` `X` `=` `homero`

Cómo obtuvo ese resultado Prolog?
---------------------------------

Básicamente lo que hace Prolog es buscar un **consecuente** (revisar al principio de todo qué era eso de consecuente) dentro de todas las cláusulas de nuestra base de conocimiento que unifique con la consulta. Podemos ver un poco la unificación o falta de unificación entre dos hechos (que serían la consulta y el consecuente) haciendo consultas en SWI que jueguen con el = (igual), para la consulta de recién tenemos:

<code> %El consecuente de la primer cláusula **NO** unifica con la consulta

?- padre(X,lisa) = padre(homero,bart).

No

%El consecuente de la segunda cláusula **NO** unifica con la consulta

?- padre(X,lisa) = padre(homero,maggie).

No

%El consecuente de la tercer cláusula **unifica** con la consulta

?- padre(X,lisa) = padre(homero,lisa).

X = homero </code>

***OJO*** el = (igual) nooooo lo vamos a usar en los programas, lo usamos acá solamente porque nos ayuda a estudiar sobre la unificación.

Este ejemplo es fácil ya que todas las cláusulas son hechos pero **no es nuestra intención entender en profundidad el mecanismo de unificación**. Si se tratara de reglas, tiene que pasar que se resuelvan los antecedentes de la cláusula cuyo consecuente unificó con la consulta; en el proceso **se agotan todas las posibles soluciones**.

Entonces, se dice que 2 términos unifican si existe algún reemplazo de todas las variables (de los 2 términos) que haga a los términos iguales; se dice que una consulta matchea con un consecuente si el predicado es el mismo y hay un reemplazo coherente que hace que todos los argumentos unifiquen. P.ej. con el reemplazo `X/homero` pasa que X queda igual que homero (primer argumento) y también que lisa es igual que lisa (segundo argumento), entonces la consulta `padre(X,lisa)` unifica con `padre(homero,lisa)` mediante el reemplazo `X/homero`.

Nos alcanza pensar que un término es un individuo (simple o compuesto - estos últimos pueden tener variables) o una consulta (como en el primer ejemplo).

`%Dos` `individuos` `iguales` `unifican` `?-` `1` `=` `1.`
`Yes`

`?-` `homero` `=` `homero.`
`Yes`

`?-` `fecha(1,1,1901)` `=` `fecha(1,1,1901).`
`Yes`

`?-` `[a,b,c]` `=` `[a,b,c].`
`Yes`

`%Existen` `reemplazos` `de` `variables` `que` `hagan` `los` `términos` `iguales` `por` `lo` `tanto` `unifican` `?-` `X` `=` `homero.`
`X` `=` `homero.`

`?-` `F` `=` `fecha(1,1,1901).`
`F` `=` `fecha(1,1,1901).`

`?-` `fecha(D,1,A)` `=` `fecha(1,M,1901).`
`D=` `1`
`M=` `1`
`A` `=` `1901`

`%Dos` `individuos` `distintos` `no` `unifican` `?-` `1` `=` `2.`
`No`

`?-` `homero` `=` `marge.`
`No`

`?-` `fecha(1,1,1901)` `=` `fecha(1,1,2010).`
`No`

`?-` `[a,b,c]` `=` `[c,b,a].`
`No`

`%No` `existen` `reemplazos` `de` `variables` `que` `hagan` `los` `términos` `iguales` `por` `lo` `tanto` `no` `unifican` `?-` `fecha(D,1,2010)` `=` `fecha(1,M,1901).` `No` `?-` `[1,2,X]` `=` `[2,2,3].`
`No`

`?-` `[1,2,3]` `=` `[X,X,3].`
`No`

Y pattern-matching?
-------------------

Bueno, la diferencia entre decir pattern-matching y unificación es bastante gris (algunos autores lo consideran sinónimos). Es muy común decir "unifica" o "matchea" indistintamente. Mayormente vamos a hablar de pattern-matching en la unificación de individuos compuestos (functores y listas - ya que estos individuos tienen un patrón definido) y vamos a hablar de unificación entre consulta y consecuente.
