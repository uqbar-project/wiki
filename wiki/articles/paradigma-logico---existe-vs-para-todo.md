---
layout: article
title: Paradigma logico   existe vs para todo
---

En la [lógica de primer orden](http://es.wikipedia.org/wiki/L%C3%B3gica_de_primer_orden) se trabaja con variables cuantificadas, los cuantificadores indican si algo se cumple para alguno (cuantificador existencial) o para todos (cuantificador universal) los valores de un conjunto de individuos.

Lo más normal es trabajar con el cuantificador existencial, motivo por el cual en el lenguaje Prolog no hay un predicado que la denote, como sí sucede para el cuantificador universal.

Si queremos definir un predicado `estaComplicado/1` que se cumpla para las personas que tienen algún hijo problemático, podríamos hacer:
```
estaComplicado(Persona):-
   padre(Persona, Hijo),
   problematico(Hijo).
```
Si luego queremos definir `estaHarto/1` de modo que se cumpla para las personas qué sólo tiene hijos problemáticos, deberíamos usar el predicado [forall](paradigma-logico---el-forall.html) para cuantificar con Para Todo a los hijos de la persona en cuestión:
```
estaHarto(Persona):-
   padre(Persona, _),  %% Generamos a la persona porque no quiero que sea una variable a cuantificar
                       %% por el para todo, sino un individuo concreto. Además restringe el universo de respuestas
                       %% a personas que tengan al menos un hijo.
   forall(padre(Persona, Hijo), problematico(Hijo)).
```

## Relación entre Existe y Para Todo

Existe una [relación entre el cuantificador universal y el existencial](http://es.wikipedia.org/wiki/Cuantificador_universal#Relaci.C3.B3n_cuantificador_universal_y_el_cuantificador_existencial), la misma se establece mediante la [negación](paradigma-logico---negacion.html).

Podríamos haber hecho una solución para `estaHarto/1` sin usar forall basándonos en esta equivalencia lógica. Alguien está harto si ninguno de sus hijos no es problemático.
```
estaHarto(Persona):-
   padre(Persona, _),
   not( (padre(Persona, Hijo), not(problematico(Hijo))) ).
```
