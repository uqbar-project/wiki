---
layout: article
title: Paradigma logico   conjuncion y disyuncion
---

Las operaciones lógicas por excelencia son la conjunción ("Y", sólo es cierta si ambas condiciones son ciertas) y la disyunción ("O", es cierta si alguna de las condiciones es cierta).

Tenemos la siguiente base de conocimiento:

```prolog
 madre(nora,luis).
 madre(nora,ana).
 madre(lidia,jose).
 madre(dora,juan).
 padre(juan,luis).
 padre(juan,ana).
 padre(juan,jose).
 padre(antonio,juan).
```

Supongamos que tenemos que desarrollar en Prolog un predicado hermano/2 que relaciona a dos personas si tienen el mismo padre y la misma madre, y otro llamado hermanastro/2 que relaciona a dos personas si tienen al padre o a la madre en común (para el ejemplo no se considerará que el O sea excluyente, por ende dos personas hermanas también serán hermanastras).

La relación de hermano, dada nuestra base de conocimientos, debería cumplirse para luis y ana, mientras que Jose es sólo hermanastro de Luis y Ana. Juan en cambio es hijo único, con lo cual no debería satisfacer ninguna de las relaciones.

Codifiquemos los predicados hermano/2 y hermanastro/2:

```prolog
 hermano(Persona,Hermano):-
   mismoPadre(Persona,Hermano),
   mismaMadre(Persona,Hermano).

 hermanastro(Persona,Hermanastro):- mismaMadre(Persona,Hermanastro).
 hermanastro(Persona,Hermanastro):- mismoPadre(Persona,Hermanastro).

 mismoPadre(Persona1,Persona2):- 
   padre(Padre,Persona1),
   padre(Padre,Persona2).

 mismaMadre(Persona1,Persona2):- 
   madre(Madre,Persona1),
   madre(Madre,Persona2).
```

La conjunción en Prolog se logra con la coma, mientras que la disyunción la conseguimos mediante la definición de varias cláusulas para el mismo predicado. Podemos ver a partir del predicado hermanastro que la búsqueda de soluciones que realiza el motor de Prolog es exhaustiva, ya que si bien jose y luis no cumplen la condición de tener la misma madre, el resultado a la consulta hermanastro(jose,luis) es verdadero, dado que continúa la evaluación con las otras alternativas para poder satisfacer la relación.

### Error común: Hacer el "O" antes de tiempo

Vamos a suponer que tenemos la siguiente lógica "una pc es gamer si tiene un disco ssd, un procesador multinúcleo, y una placa de video shifors 6 o una reidion 8"
El siguiente código está mal, porque **repite lógica**:

```prolog
esGamer(PC):-
  tieneDiscoSSD(PC),
  tieneMultiNucleo(PC),
  placa(PC,shifors6).
  
esGamer(PC):-
  tieneDiscoSSD(PC),
  tieneMultiNucleo(PC),
  placa(PC,reidion8).
  
 % ¡REPITE LÓGICA!
```

La definición de una pc gamer **debe ser única**, es decir, tiene que tener _una sola cláusula_. ¡Hay una sola forma de que una pc sea gamer, no dos! 
¿Cómo? ¡Nos falta una **abstracción**! La abstracción `tienePlacaBuena`. Implícitamente esa idea está, pero nos falta en nuestro código. Veamos cómo agregando esa abstracción evitamos la repetición de lógica: 

> "una pc es gamer si tiene un disco ssd, un procesador multinúcleo, y **una buena placa de video**"

```prolog
esGamer(PC):-
  tieneDiscoSSD(PC),
  tieneMultiNucleo(PC),
  tienePlacaBuena(PC).
  
tienePlacaBuena(PC):-
  placa(PC,shifors6).
  
tienePlacaBuena(PC):-
  placa(PC,reidion8).

% No repite lógica: la definición de "esGamer" es única, delega en otro predicado "tienePlacaBuena" los detalles.
```
