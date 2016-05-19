El paradigma lógico se basa en la definición de reglas lógicas y es un paradigma declarativo así que si no lo leíste todavía te recomiendo leer antes sobre [Declaratividad](declaratividad.html). En este artículo hablamos principalmente de la teoría del paradigma lógico, pero para los ejemplos tomamos como base el lenguaje [Prolog](prolog.html)

Introducción: Silogismos
------------------------

¿Cómo describir un problema usando la lógica? ¿Qué tipo de problema podemos describir?

Comencemos con un ejemplo simple, seguramente muchos de ustedes lo habrán escuchado alguna vez:

  
*Sócrates es hombre*

*Todos los hombres son mortales*

Luego... ¿qué podemos deducir de esto? Claro adivinaste, que Sócrates es mortal. De eso se trata la programación lógica, vamos a describir nuestro conocimiento en formas de reglas y vamos a permitir que otra cosa (el [motor](declaratividad-elementos-en-un-programa-declarativo.html)) se ocupe de procesar ese conocimiento y sacar conclusiones al respecto.

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

-   `hombre(socrates)` afirma que Sócrates es un hombre, o dicho de otra manera que *socrates* tiene la característica *hombre*. Es una afirmación que afecta únicamente a Sócrates y la llamamos un **hecho**, ya que es una declaración que no depende de nada para ser verdadera.
-   `mortal(X):-hombre(X)` es lo que llamamos una **regla** y se puede leer como: *X es hombre ⇒ X es mortal*. La regla es una implicación, el antecedente es `hombre(X)` y el consecuente es `mortal(X)`. Esto quiere decir que para todo X que tenga la característica *hombre* se da que ese X también tiene la característica *mortal* (o más corto: todos los hombres son mortales).

Fijate que *socrates* está en minúscula, mientras que la *X* aparece en mayúscula, ¿por qué? Los términos en minúscula se refieren a cosas particulares y las palabras en mayúscula son **incógnitas** (o variables).

Eso se relaciona también con que la primera línea sea un hecho (porque habla de un individuo particular) y la segunda sea una regla (porque habla de todos los hombres).

Pero la principal diferencia entre el hecho y la regla es que la regla tiene un antecedente (que se debe cumplir para que se cumpla la regla) y el hecho no, el hecho es verdadero siempre. En programas más complejos, a veces pasa que tenemos hechos que usan variables o reglas sobre individuos particulares.

Los hechos me permiten definir **por extensión** el conjunto de individuos que tienen una característica. Las reglas me permiten hacer esa misma definición **por comprensión**.

Un poco de teoría
-----------------

Si entendiste todo hasta acá, es momento de formalizar algunas cositas acerca del programa que hicimos. Antes que nada,

-   la base de conocimiento se compone de **cláusulas** que definen **predicados** partiendo de los **individuos** de los que queremos hablar.
-   un programa Prolog es una base de conocimiento

Con esto claro, podemos pasar a ver algunas [definiciones](paradigma-logico---un-poco-de-nomenclatura.html)

Estos son los conceptos principales de los que se compone un programa Prolog. Para obtener resultados a partir de un programa, hacemos [consultas](paradigma-logico---un-poco-de-nomenclatura-consultas.html).

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

En este momento aparece un concepto que llamamos **principio de universo cerrado**, que dice que el motor asume como falso todo lo que no pueda probar como verdadero, es decir que si al preguntarle si aristoteles es mortal, me va a contestar que no!

Muchos entornos (tanto dentro del paradigma lógico como en otros muchos lugares) trabajan con este principio. Aunque no es la única forma de trabajar, es algo bastante frecuente dado que lo contrario es en general más complicado de implementar y de utilizar.

*¿Cómo solucionar el problema?*

Agregando el hecho que indica que Aristóteles es un hombre: <code>

    hombre(aristoteles).

</code> Si volvemos a hacer la misma consulta ahora vamos a tener el resultado esperado.
