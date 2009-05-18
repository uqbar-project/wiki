== Base de conocimiento = programa de ejemplo ==

` hombre(socrates).`
` hombre(solon).`
` hombre(arquimedes).`
` mortal(X):-hombre(X).`
` ciudad(atenas).`
` ciudad(siracusa).`
` vive(socrates,atenas).`
` vive(solon,atenas).`
` vive(arquimedes,siracusa).`
` nacio(solon,-634).`
` nacio(arquimedes,-287).`
` sonConciudadanos(P1,P2):- vive(P1,C), vive(P2,C).`

Individuos
----------

Los individuos son aquellas cosas sobre las que versa el conocimiento que queremos expresar. En el ejemplo aparecen varios individuos: socrates, atenas, solon, arquimedes, siracusa, -634, -287. Individuo es cualquier entidad acerca de la cual nos interese estudiar sus características o sus relaciones con otros individuos. P.ej. si hiciéramos un sistema para controlar correlatividades entre las materias de la facultad, tendríamos un individuo para representar cada materia. Los individuos se dividen en

-   **Individuos simples**: los átomos (como solon, atenas, siracusa) y los números son individuos simples.
-   **Individuos compuestos**: tienen otros individuos adentro, los componentes, y se pueden ver o bien como una unidad o bien acceder a cada componente. Ver [Paradigma Lógico - individuos compuestos](paradigma-logico---individuos-compuestos.html)

Predicados
----------

Los predicados son las cosas que queremos decir (o predicar, je) acerca de los invividuos. En este ejemplo los predicados que aparecen son: mortal, hombre, vive, nacio, sonConciudadanos. Volviendo al ejemplo del sistema de correlatividades, seguramente tendría un predicado que relacione cada alumno con cada materia que cursó, y otro que indique la correlatividad entre dos materias relacionando una materia con cada requisito.

A la cantidad de parámetros que lleva cada predicado la llamamos su **aridad**. En el ejemplo, los predicados hombre, mortal y ciudad tienen aridad 1, mientras que vive y sonConciudadanos tienen aridad 2. A partir de su aridad podemos separar los predicados en:

-   Propiedades : Son los predicados de aridad 1, que expresan características de individuos.
-   Relaciones : Son los predicados de aridad mayor a 1, que expresan relaciones entre individuos.

Cláusulas
---------

Cada una de las sentencias = unidades de información de una base de conocimiento. Las cláusulas deben terminar con un punto `.` El ejemplo tiene 12 cláusulas. Cada cláusula participa en la definición de un predicado, define ciertos casos para los que un predicado se verifica. En el ejemplo:

-   las cláusulas 1 a 3 definen por extensión el predicado hombre.
-   la cláusula 4 define el predicado mortal, indicando que cualquier individuo que sea hombre, es mortal.
-   las cláusulas 5 y 6 definen por extensión el predicado ciudad
-   etc..

Cada cláusula puede ser:

-   un **hecho**: hace una afirmación incondicional = sin condiciones, generalmente sobre un individuo particular. En el ejemplo todas las cláusulas son hechos salvo las que definen mortal y sonConciudadanos. Sintácticamente, los hechos son las claúsulas que no incluyen el símbolo `:-` .
-   una **regla**: define una implicación, es decir que define que si se cumplen ciertas condiciones, entonces un predicado se verifica para ciertos individuos. En la cláusula

` mortal(X):- hombre(X).`

la condición es que un cierto X sea mortal, y lo que definimos es que si se cumple la condición entonces el predicado mortal se cumple para el mismo X. Una regla se compone de una **cabeza** (`mortal(X)`) y un **cuerpo** (`hombre(X)`), unidos por el símbolo :- que denominamos **cuello**. Si vemos una regla como una implicación con antecedente y consecuente, está invertida respecto a lo que se vio al estudiar lógica: la cabeza es el consecuente, el cuerpo es el antecedente.

Consultas
---------

Son la forma de usar un programa en lógico, se hace una consulta, y se obtiene una o varias respuestas.

Existen dos tipos de consulta:

<table>
<tbody>
<tr class="odd">
<td><p><strong>Individuales</strong></p></td>
<td><p>se hacen sobre individuos específicos. Por ejemplo:</p>
<p><code> ?- mortal(socrates).</code><br />
<code> ?- sonConciudadanos(socrates,solon).</code></p></td>
</tr>
<tr class="even">
<td><p><strong>Existenciales</strong></p></td>
<td><p>se buscan individuos que satisfagan la relación. En este caso una consulta puede tener muchas respuestas. Por ejemplo:</p>
<p><code> ?- mortal(X).</code><br />
<code> ?- sonConciudadanos(X,Y).</code><br />
<code> ?- sonConciudadanos(solon,Y).</code></p>
<p>En el segundo caso cada respuesta es un par de conciudadanos (o sea, de individuos relacionados por el predicado <code>sonConciudadanos</code>, mientras que en el tercero cada respuesta es un conciudadano de Solón. En este tipo de consultas aparecen <a href="Paradigma_lógico_-_variables" title="wikilink">variables o incógnitas</a>. Por ese motivo este tipo de consultas también son llamadas <em>consultas variables</em>.</p></td>
</tr>
</tbody>
</table>

El paradigma lógico trabaja con el principio de [Universo Cerrado](paradigma-logico---introduccion-universo-cerrado.html).
