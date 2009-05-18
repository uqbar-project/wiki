El paradigma lógico se basa en la definición de reglas lógicas y es un paradigma declarativo así que si no lo leíste todavía te recomiendo leer antes sobre [Declaratividad](declaratividad.html). En este artículo hablamos principalmente de la teoría del paradigma lógico, pero para los ejemplos tomamos como base el lenguaje [Prolog](prolog.html)

Introducción: Silogismos
------------------------

¿Cómo describir un problema usando la lógica? ¿Qué tipo de problema podemos describir?

Comencemos con un ejemplo simple, seguramente muchos de ustedes lo habrán escuchado alguna vez:

  
*Sócrates es hombre*

*Todos los hombres son mortales*

Luego... ¿qué podemos deducir de esto? Claro adivinaste, que Sócrates es mortal. De eso se trata la programación lógica, vamos a describir nuestro conocimiento en formas de reglas y vamos a permitir que otra cosa (el [motor](motor.html)) se ocupe de procesar ese conocimiento y sacar conclusiones al respecto.

¿Y solito se da cuenta de las cosas? Más o menos, en realidad no podemos pedirle al motor que solamente se ponga a deducir y que nos diga a qué conclusión llega, hay que hacerle preguntas más concretas. Por ejemplo podemos preguntarle

  
si socrates es mortal, y nos va a decir que sí

qué mortales conoce, y nos va a decir que socrates es mortal

Esos son los dos tipos de preguntas básicas que el motor va a saber contestar, después vamos a bajar en detalle sobre esto.

¿Cómo lo bajamos a código?
--------------------------

Programemos este mismo ejemplo en Prolog, en realidad es bastante simple: <code>

    hombre(socrates).
    mortal(X):-hombre(X).

</code>

¿Qué quiere decir esto?

-   `hombre(socrates)` afirma que Sócrates es un hombre, o dicho de otra manera que *socrates* tiene la característica *hombre*. Es una afirmación que afecta únicamente a Sócrates y la llamamos un [hecho](hecho.html).
-   `mortal(X):-hombre(X)` es lo que llamamos una [regla](regla.html) y se puede leer como: *X es hombre ⇒ X es mortal*. La regla es una implicación, el antecedente es `hombre(X)` y el consecuente es `mortal(X)`. Esto quiere decir que para todo X que tenga la característica *hombre* se da que ese X también tiene la característica *mortal* (o más corto: todos los hombres son mortales).

Fijate que *socrates* está en minúscula, mientras que la *X* aparece en mayúscula, ¿por qué? Los términos en minúscula se refieren a cosas particulares y las palabras en mayúscula son [incógnitas](incognita.html) (o variables).

Eso se relaciona también con que la primera línea sea un hecho (porque habla de un individuo particular) y la segunda sea una regla (porque habla de todos los hombres).

Pero la principal diferencia entre el hecho y la regla es que la regla tiene un antecedente (que se debe cumplir para que se cumpla la regla) y el hecho no, el hecho es verdadero siempre. En programas más complejos, a veces pasa que tenemos hechos que usan variables o reglas sobre individuos particulares.

Los hechos me permiten definir **por extensión** el conjunto de individuos que tienen una característica. Las reglas me permiten hacer esa misma definición **por comprensión**.

Para más información, ver la [Sintaxis de Prolog](sintaxis-de-prolog.html)

Un poco de teoría
-----------------

Si entendiste todo hasta acá, es momento de formalizar algunas cositas acerca del programa que hicimos. Antes que nada,

-   la base de conocimiento se compone de **cláusulas** que definen **predicados** partiendo de los **individuos** de los que queremos hablar.
-   un programa Prolog es una base de conocimiento

Armemos una base de conocimiento = programa un poco más grande

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

Ahora sí, las definiciones

<table>
<tbody>
<tr class="odd">
<td><p><a href="individuo" title="wikilink">individuos</a></p></td>
<td><p>Los individuos son aquellas cosas sobre las que versa el conocimiento que queremos expresar. En el ejemplo aparecen varios individuos: socrates, atenas, solon, arquimedes, siracusa, -634, -287. Individuo es cualquier entidad acerca de la cual nos interese estudiar sus características o sus relaciones con otros individuos. P.ej. si hiciéramos un sistema para controlar correlatividades entre las materias de la facultad, tendríamos un individuo para representar cada materia. Los individuos se dividen en</p>
<ul>
<li><strong>Individuos simples</strong>: los átomos (como solon, atenas, siracusa) y los números son individuos simples.</li>
<li><strong>Individuos compuestos</strong>: tienen otros individuos adentro, los componentes, y se pueden ver o bien como una unidad o bien acceder a cada componente. Ver <a href="Paradigma_Lógico_-_individuos_compuestos" title="wikilink">Paradigma Lógico - individuos compuestos</a></li>
</ul></td>
</tr>
<tr class="even">
<td><p><a href="predicado" title="wikilink">predicados</a></p></td>
<td><p>Los predicados son las cosas que queremos decir (o predicar, je) acerca de los invividuos. En este ejemplo los predicados que aparecen son: mortal, hombre, vive, nacio, sonConciudadanos. Volviendo al ejemplo del sistema de correlatividades, seguramente tendría un predicado que relacione cada alumno con cada materia que cursó, y otro que indique la correlatividad entre dos materias relacionando una materia con cada requisito.</p>
<p>Cada predicado tiene una <strong>aridad</strong>, que es la cantidad de argumentos que relaciona. En el ejemplo, los predicados hombre, mortal y ciudad tienen aridad 1, mientras que vive y sonConciudadanos tienen aridad 2. La clasificación de los predicados según su aridad es interesante para comprender el signifidado de un programa:</p>
<ul>
<li><a href="Predicado_Monádico" title="wikilink">Predicado Monádico</a> (un único parámetro) : Expresan características de los individuos.</li>
<li><a href="Predicado_Poliádico" title="wikilink">Predicado Poliádico</a> (dos o más parámetros) : Expresan relaciones entre los individuos.</li>
</ul></td>
</tr>
<tr class="odd">
<td><p><a href="cláusula" title="wikilink">cláusulas</a></p></td>
<td><p>Cada una de las sentencias = unidades de información de una base de conocimiento. Las cláusulas deben terminar con un punto <code>.</code> El ejemplo tiene 12 cláusulas. Cada cláusula participa en la definición de un predicado, define ciertos casos para los que un predicado se verifica. En el ejemplo:</p>
<ul>
<li>las cláusulas 1 a 3 definen por extensión el predicado hombre.</li>
<li>la cláusula 4 define el predicado mortal, indicando que cualquier individuo que sea hombre, es mortal.</li>
<li>las cláusulas 5 y 6 definen por extensión el predicado ciudad</li>
<li>etc..</li>
</ul>
<p><br />
Cada cláusula puede ser:</p>
<ul>
<li>un <strong>hecho</strong>: hace una afirmación incondicional = sin condiciones, generalmente sobre un individuo particular. En el ejemplo todas las cláusulas son hechos salvo las que definen mortal y sonConciudadanos. Sintácticamente, los hechos son las claúsulas que no incluyen el símbolo <code>:-</code> .</li>
<li>una <strong>regla</strong>: define una implicación, es decir que define que si se cumplen ciertas condiciones, entonces un predicado se verifica para ciertos individuos. En la cláusula</li>
</ul>
<p><code> mortal(X):- hombre(X).</code></p>
<p>la condición es que un cierto X sea mortal, y lo que definimos es que si se cumple la condición entonces el predicado mortal se cumple para el mismo X. Una regla se compone de una <strong>cabeza</strong> (<code>mortal(X)</code>) y un <strong>cuerpo</strong> (<code>hombre(X)</code>), unidos por el símbolo :- que denominamos <strong>cuello</strong>. Si vemos una regla como una implicación con antecedente y consecuente, está invertida respecto a lo que se vio al estudiar lógica: la cabeza es el consecuente, el cuerpo es el antecedente.</p></td>
</tr>
</tbody>
</table>

Estos son los conceptos principales de los que se compone un programa Prolog. Para obtener resultados a partir de un programa, hacemos [consultas](paradigma-logico-tipos-de-consulta.html).

Universo Cerrado
----------------

*¿Qué pasa si ahora quiero preguntar si Aristóteles es mortal?* <code>

    ?- mortal(aristoteles).

</code>

Al ejecutar esa consulta, la secuencia de pasos del motor (simplificada) es la siguiente:

1.  El motor buscará en la base de conocimientos las diferentes cláusulas del predicado `mortal/1`, en particular las que matcheen con `mortal(aristoteles)`.
2.  Al hacer esto encontrará una única regla: `mortal(X):-hombre(X)`. Ergo, para probar que Aristóteles es mortal, deberá probar que es hombre.
3.  Al intentar verificar si aristoteles es un hombre, es decir la consulta `hombre(aristoteles)`.
4.  Pero la única definición del predicado hombre es `hombre(socrates)`, que no matchea con `hombre(aristoteles)`.
5.  La base de conocimientos no dice nada acerca de aristóteles, por lo tanto no se puede verificar que Aristóteles sea mortal.

En este momento aparece un concepto que llamamos [principio de universo cerrado](principio-de-universo-cerrado.html), que dice que el motor asume como falso todo lo que no pueda probar como verdadero, es decir que si al preguntarle si aristoteles es mortal, me va a contestar que no!

Muchos entornos (tanto dentro del paradigma lógico como en otros muchos lugares) trabajan con este principio. Aunque no es la única forma de trabajar, es algo bastante frecuente dado que lo contrario es en general más complicado de implementar y de utilizar.

*¿Cómo solucionar el problema?*

Agregando el hecho que indica que Aristóteles es un hombre: <code>

    hombre(aristoteles).

</code> Si volvemos a hacer la misma consulta ahora vamos a tener el resultado esperado.
