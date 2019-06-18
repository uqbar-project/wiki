---
layout: article
title: Paradigma logico   un poco de nomenclatura
---

Cuando describimos los elementos de un programa en lógico (ya sea para su definición como su uso) vamos a usar los siguientes términos:

1.  Individuo
    -   Individuo simple
        -   Átomo
        -   Número
    -   Individuo compuesto
        -   Lista
        -   Functor

2.  Predicado
    -   Propiedad
    -   Relación

3.  Cláusula
    -   Hecho
    -   Regla

4.  Consulta
    -   Individual
    -   Existencial
    
A continuación mostramos un programa sencillo como para tener de referencia e identificar cuándo usamos cada uno de estos términos.

## Base de conocimiento = programa de ejemplo
```
hombre(socrates).
hombre(solon).
hombre(pericles).
hombre(arquimedes).
mortal(X):-
  hombre(X).
ciudad(atenas).
ciudad(siracusa).
vive(socrates,atenas).
vive(solon,atenas).
vive(pericles,atenas).
vive(arquimedes,siracusa).
nacio(solon,-634).
nacio(pericles,-495).
nacio(arquimedes,-287).
sonConciudadanos(P1,P2):- 
  vive(P1,C), 
  vive(P2,C).
```

Individuos
----------

Los individuos son aquellas cosas sobre las que versa el conocimiento que queremos expresar. En el ejemplo aparecen varios individuos: `socrates`, `atenas`, `solon`, `arquimedes`, `siracusa`, `-634`, `-287`. Individuo es cualquier entidad acerca de la cual nos interese estudiar sus características o sus relaciones con otros individuos. P.ej. si hiciéramos un sistema para controlar correlatividades entre las materias de la facultad, tendríamos un individuo para representar cada materia. Los individuos se dividen en

-   **Individuos simples**: los átomos (como `solon`, `atenas`, `siracusa`) y los números son individuos simples. Ver [Paradigma Lógico - individuos simples](paradigma-logico---individuos-simples.html)
-   **Individuos compuestos**: tienen otros individuos adentro, los componentes, y se pueden ver o bien como una unidad o bien acceder a cada componente mediante pattern matching. Ver [Paradigma Lógico - individuos compuestos](paradigma-logico---individuos-compuestos.html).

Predicados
----------

Los predicados son las cosas que queremos decir (o predicar, je) acerca de los invividuos. En este ejemplo los predicados que aparecen se llaman mortal, hombre, vive, nacio, sonConciudadanos.

A la cantidad de parámetros que lleva cada predicado la llamamos su **aridad**. En el ejemplo, los predicados hombre, mortal y ciudad tienen aridad 1, mientras que vive y sonConciudadanos tienen aridad 2. A partir de su aridad podemos separar los predicados en:

-   **Propiedades** : Son los predicados de aridad 1, que expresan características de individuos. Por ejemplo, ser mortal es una propiedad.
-   **Relaciones** : Son los predicados de aridad mayor a 1, que expresan relaciones entre individuos. Por ejemplo, saber dónde vive alguien es una relación entre una persona y una ciudad.

Usualmente haremos referencia a un predicado en particular, no sólo mediante su nombre sino también su aridad (por ejemplo `hombre/1`), ya que podrían haber predicados con el mismo nombre pero distinta aridad y serían totalmente independientes entre ellos.

Volviendo al ejemplo del sistema de correlatividades, seguramente tendría un predicado que relacione cada alumno con cada materia que cursó, y otro que indique la correlatividad entre dos materias relacionando una materia con cada requisito.

Cláusulas
---------

Cada una de las sentencias = unidades de información de una base de conocimiento. Las cláusulas deben terminar con un punto `.` El ejemplo tiene 15 cláusulas. Cada cláusula participa en la definición de un predicado, define ciertos casos para los que un predicado se verifica. En el ejemplo:

-   las cláusulas 1 a 4 definen por extensión el predicado `hombre/1`.
-   la cláusula 5 define el predicado `mortal/1`, indicando que cualquier individuo que sea hombre, es mortal.
-   las cláusulas 6 y 7 definen por extensión el predicado `ciudad/1`
-   etc..

Cada cláusula puede ser un **hecho** o una **regla**.

Un **hecho** hace una afirmación incondicional (no depende de ninguna condición para ser cierta), generalmente sobre un individuo particular.

En el ejemplo todas las cláusulas son hechos salvo las que definen los predicados `mortal/1` y `sonConciudadanos/2`. Sintácticamente, los hechos son las claúsulas que no incluyen el símbolo `:-` .

Una **regla** define una implicación, es decir que define que si se cumplen ciertas condiciones, entonces un predicado se verifica para ciertos individuos.

La cláusula `mortal(X):- hombre(X).` sirve para determinar de forma general (por comprensión, no por extensión) si un cierto X es mortal. Esta definición indica que si se cumple la condición `hombre(X)` entonces el predicado `mortal/1` se cumple para ese mismo X. Una regla se compone de una **cabeza** (`mortal(X)`) y un **cuerpo** (en este caso es solamente `hombre(X)`), unidos por el símbolo :- que denominamos **cuello**. Si vemos una regla como una implicación con antecedente y consecuente, está invertida respecto a lo que se vio al estudiar lógica: la cabeza es el consecuente, el cuerpo es el antecedente.

El predicado `sonConciudadanos/2` también se define con una regla, sólo que un poco más compleja porque depende de una conjunción entre dos condiciones más sencillas.

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
<td><p>Se verifica si <em>existe algún individuo</em> que satisfaga la consulta. Además, Prolog a veces nos sabe dar &quot;ejemplos&quot; de individuos que hacen verdadera la consulta. Por ejemplo:</p>
<p><code> ?- mortal(X).</code><br />
<code> ?- sonConciudadanos(X,Y).</code><br />
<code> ?- sonConciudadanos(solon,Y).</code></p>
<p>En el segundo caso cada respuesta es un par de conciudadanos (o sea, de individuos relacionados por el predicado <code>sonConciudadanos/2</code>, mientras que en el tercero cada respuesta es un conciudadano de Solón.<br />
En este tipo de consultas aparecen variables o incógnitas. Por ese motivo este tipo de consultas también son llamadas <em>consultas variables</em>.</p></td>
</tr>
</tbody>
</table>
