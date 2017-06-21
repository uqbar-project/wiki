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
