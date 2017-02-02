---
layout: article
title: Paradigma logico   forall   no siempre con member
---

Partimos de la base que el forall corresponde a la idea "a todos los que les pasa A, les pasa B".

Por alguna razón, una proporción importante de las personas que están aprendiendo el paradigma lógico tienden a pensar que cada vez que quiero usar forall, tengo que tener la lista de los que les pasa A, entonces siempre que aparece forall como condición en una regla, va a ser de esta forma

`   forall(member(E,ListaDeLosQueLePasaA), ...condiciones B sobre E...)`

Tal vez esto está relacionado con asimilar el forall con un for de (ponele) Pascal, que recorre la ListaDeLosQueLePasaA y verifica las condiciones del 2do argumento del forall para cada elemento de la lista.

Esta forma de pensarlo **no es correcta**, veamos dos formas de ver la falla de razonamiento.

1. no conviene pensar en el forall de acuerdo a "lo que hace", sino a "cuándo se verifica", o sea, si pongo forall como condición de una regla, cuando se va a cumplir.

2. el forall se cumple cuando cada respuesta a su primer argumento (que es una consulta) es respuesta del segundo argumento (que es otra consulta). Entonces, si uso forall poniendo member como primer consulta, lo que estoy diciendo es que todos los elementos de la lista cumplen con la 2da consulta, tiene el mismo resultado de "recorrer todos los elementos", pero la idea es otra. Entonces, puedo usar la idea con member, o con cualquier condición que quiera.

Un caso particular en el que se nota este razonamiento inadecuado es cuando se usa un findall seguido de un member, de esta forma

`   findall(X, ...condiciones A sobre X..., ListaDeX), forall(member(E,ListaDeX), ...condiciones B sobre E...)`

es más directo, claro y correcto, usar el forall poniendo directamente "a E le pasa A" en lugar de "E está en la lista de los que les pasa A", en Prolog:

`   forall(...condiciones A sobre X..., ...condiciones B sobre E...)`

Veamos un ejemplo: quiero decir que un tipo es feliz si todos sus amigos son buena onda, tenemos los predicados amigos/2 y esBuenaOnda/1. Si quiero definir

`  esFeliz(Pers)`

hay una condición "para todos los A pasa B" en donde calza justo un forall, "A" es "ser amigo de Pers" y "B" es "ser buena onda".

Si pienso que forall me sirve solamente para recorrer listas, entonces tengo que armar la lista de los amigos de Pers, y después fijarme para cada elemento que sea buena onda. En Prolog:

`   esFeliz(Pers):- `
`       findall(Amigo,amigos(Pers,Amigo),AmigosDePers), `
`       forall(member(Chabon,AmigosDePers), esBuenaOnda(Chabon)).`

si entiendo que el primer argumento de forall puede ser cualquier consulta, pongo lo que entiendo de la definición, que es "ser amigo de Pers". En Prolog

`   esFeliz(Pers):- forall(amigo(Pers,Amigo), esBuenaOnda(Amigo)).`
