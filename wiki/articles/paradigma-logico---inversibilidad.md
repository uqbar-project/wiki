---
layout: article
title: Paradigma logico   inversibilidad
---

Definición
----------

La inversibilidad es una de las características distintivas del paradigma lógico, que no aparece en los paradigmas funcional ni objetos. Que un predicado sea inversible significa que los argumentos del mismo no tienen una "dirección". Es decir, no necesitás "pasar" el parámetro resuelto (o sea, pasás un valor), sino que podés pasarlo sin unificar (o sea, pasás una incógnita).

Imaginate un predicado `esNatural/1` que te dice si un número es natural:

    esNatural(1)
    Yes

    esNatural(-2)
    No

Cuando un predicado es inversible, también podés hacer otro tipo de consultas, poniendo una incógnita en el parámetro en lugar de un valor:

    esNatural(N)
    N=1;
    N=2;
    N=3;
    etc...

Esta característica es exclusiva del paradigma lógico, ya que está basado en el concepto de relación matemática (y no en el de función).

La inversibilidad no tiene que ver solamente con que "dé error", muchas veces lo que pasa es -que sin dar error- se comporta incorrectamente o de forma distinta a la que uno espera, en ese caso también decimos que no es inversible.

### Ejemplos

Veamos este programa

`  vive(ruben,lanus).`
`  vive(ana,lanus).`
`  vive(laura,boedo).`
`  vive(susi,bernal).`
`  `
`  sonVecinos(P1,P2):- vive(P1,B), vive(P2,B), P1 \= P2.`
`  esDelSur(P):- vive(P,lanus).`
`  esDelSur(P):- vive(P,bernal).`

Todos los predicados que aparecen son totalmente inversibles, porque puedo hacer consultas con cualquier combinación de valores e incógnitas. ¿Qué quiere decir esto? Mirá todas las consultas que se pueden hacer

`  ?- vive(ruben,B).`
`  ?- vive(P,lanus).`
`  ?- vive(ruben,boedo).   % responde que no`
`  ?- vive(P,B).`

y lo mismo para sonVecinos

`  ?- sonVecinos(ruben,X).`
`  ?- sonVecinos(X,ruben).`
`  ?- sonVecinos(ana,ruben).   % responde que sí`
`  ?- sonVecinos(X,Y).`

para esDelSur hay menos combinaciones, porque tiene un solo argumento

`  ?- esDelSur(susi).     % ¿qué te parece que responde?`
`  ?- esDelSur(P).        % idem`

Observar que cada predicado tiene una definición sola, no tengo que pensar en distintas definiciones para cubrir las distintas combinaciones entre valores e incógnitas. Para sonVecinos, no tengo que armar una regla por si me consultan con dos valores, una distinta por si me consultan con dos incógnitas, etc..

OJO - inversible no es lo mismo que simétrico
---------------------------------------------

Sigamos con el programa anterior. Ya vimos que los tres predicados son **inversibles**.

Ahora bien, ¿son **simétricos**?

Empecemos pensando qué quiere decir "simétricos". Es como una relación simétrica de las de teoría de conjuntos: una relación R es simétrica si siempre que pasa aRb, también pasa bRa.

Entonces, el predicado vive/2, ¿es simétrico? Veamos. Si consulto

`  ?- vive(ruben,lanus).   `

responde que sí, o sea, ruben / vive / lanus. En el esquema aRb, es

-   a: ruben
-   R: vive
-   b: lanus

O sea que bRa sería

`  ?- vive(lanus,ruben).   `

y la respuesta de esto claramente va a ser no.

Entonces el predicado vive/2 no es simétrico. Pero ya vimos que sí es inversible, porque

-   inversible quiere decir "puedo hacer consultas con cualquier combinación de valores e incógnitas".
-   y **no** quiere decir "si invierto los argumentos da las mismas respuestas". Esto es simetría.

En cambio, sonVecinos/2 es esperable que se verifique para el mismo par de individuos en cualquier orden. A su vez, por como está definido es inversible, pero no tiene nada que ver una cosa con la otra.

Observar que la noción de simetría sólo se aplica a predicados de 2 argumentos (para esDelSur/1 ¿cómo sería "ser simétrico"? No tiene sentido), mientras que la de inversibilidad se aplica a cualquier predicado.

Resumen ejecutivo: ojo al piojo, inversibilidad es una cosa muy distinta a simetría, no confundirse. Si no queda claro, consulte con su docente amigo. Lo que nos va a interesar en la materia es el concepto de **inversibilidad**.

¿Cómo hacés que un predicado sea inversible?
--------------------------------------------

En principio, todo predicado es inversible salvo que caiga en un [caso de no-inversibilidad](paradigma-logico---casos-de-no-inversibilidad.html).

Estos casos tienen que ver con las submetas de un predicado que requieren variables ligadas, en estas cosas es que hay que fijarse para saber qué hace falta [generar](paradigma-logico---generacion.html).

Por ejemplo: cuando una de las componentes de un predicado es un not/1 , necesitás que el predicado que le mandás tenga sus parámetros unificados. Cuando usás una evaluación matemática (ej: X is N \* 2), necesitás que lo de la derecha (la N) esté unificado.

¿Cuál sería el uso?
-------------------

Inherentemente es más potente que una función, ya que te permite encontrar todos los valores para los cuales una relación se cumple (y esto tiene que ver también con la idea de [ múltiples resultados](paradigma-logico---multiples-respuestas.html)...).

La ventaja inmediata es que te permite más formas de usarlo, lo típico: puedo preguntar si un alumno aprobó, o todos los alumnos que aprobaron, etc.

Luego podría aparecer otra ventaja y es que si todos mis predicados son inversibles, eso de verdad me permite usarlos sin pensar nada nada nada en la secuencia y me da más lugar a sólo declarar el conocimiento. Y eso le da una potencia más grande a mi lenguaje/paradigma/entorno de programación.

(Igual no debe entenderse de lo último que todos los predicados que hago deben ser inversibles. )
