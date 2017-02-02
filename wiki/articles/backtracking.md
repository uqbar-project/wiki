---
layout: article
title: Backtracking
---

En un programa desarrollado usando el paradigma lógico, existe un “motor” que actúa como control de secuencia para separar el algoritmo de búsqueda de soluciones del conocimiento de cada programa.

Durante la ejecución de un programa va evaluando y combinando las reglas lógicas de la base de conocimiento para lograr los resultados esperados. La implementación del mecanismo de evaluación puede ser diferente en cada lenguaje del paradigma, pero en todos los casos debe garantizar que se agoten todas las combinaciones lógicas posibles para ofrecer el conjunto completo de respuestas alternativas posibles a cada consulta efectuada. El más difundido se denomina **backtracking**, que utiliza una estrategia de búsqueda de soluciones en estructuras de árboles denominada *primero en profundidad*.

El motor de Prolog usa el mecanismo de backtracking para encontrar soluciones a una consulta. Supongamos que tenemos la siguiente base de conocimientos:

`hijo(homero,bart).`
`hijo(homero,maggie).`
`hijo(homero,lisa).`

`item(bart,patineta).`
`item(bart,gomera).`
`item(lisa,saxo).`

`copado(patineta).`
`copado(saxo).`

`itemCopadoDeHijo(Persona,Item):- hijo(Persona,Hijo),`
` item(Hijo,Item), copado(Item).`

Si hacemos la consulta existencial:

`?- itemCopadoDeHijo(P,I).`
`P = homero   I = patineta;`
`P = homero   I = saxo;`
`No.`

¿Cómo se llega a esa solución?

El motor hace la consulta hijo(Persona,Hijo) que tiene 3 respuestas posibles en base a nuestros hechos. Por cada una de esas soluciones posibles iniciales tendrá diferentes caminos por los cuales avanzar.

1- Si Persona es homero e Hijo es bart: Se consulta item(bart,Item), que tiene dos respuestas posibles.

1.1- Si Item es patineta, copado(patineta) se verifica, con lo cual es respuesta para itemCopadoDeHijo/2

1.2- Si Item es gomera, copado(patineta) no se verifica, con lo cual no es respuesta para itemCopadoDeHijo/2

2- Si Persona es homero e Hijo es maggie: Se consulta item(maggie,Item) que no se verifica, con lo cual no se avanza más por este camino.

3- Si Persona es homero e Hijo es lisa: Se consulta item(lisa,Item), que tiene una respuesta posible.

3.1 - Si Item se liga con saxo, copado(saxo) es cierto, con lo cual es respuesta para itemCopadoDeHijo/2
