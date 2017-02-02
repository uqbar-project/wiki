---
layout: article
title: Paradigma logico   como pienso la resolucion de un punto
---

Ejemplo
-------

    tour(lokotour,[tramo(madrid,roma), tramo(roma,bonn), tramo(bruselas,burdeos)]).
    tour(eurobleh,[tramo(madrid,perpignan), tramo(perpignan,paris), tramo(paris,praga)]).

    pais(bsas,argentina).
    pais(mendoza,argentina).
    pais(lima,peru).
    pais(quito,ecuador).
    pais(caracas,venezuela).
    pais(toronto,canada).
    pais(montreal,canada).

    pais(madrid,espania).
    pais(toledo,espania).
    pais(roma,italia).
    pais(bonn,alemania).
    pais(bruselas,belgica).
    pais(burdeos,francia).
    pais(perpignan,francia).
    pais(paris,francia).
    pais(praga,repcheca).

    pais(sydney, australia).

    ciudadGrosa(bsas).
    ciudadGrosa(lima).
    ciudadGrosa(quito).
    ciudadGrosa(caracas).
    ciudadGrosa(montreal).
    ciudadGrosa(toledo).

Qué **no** hago
---------------

Pensar en recorrer, no hay que recorrer nada.

Qué **sí** hago
---------------

Si tengo que crear un predicado nuevo, pienso en la aridad, y qué representa cada argumento.

  
P.ej. el predicado pasaPorPais relaciona un tour con un país, entonces es de aridad 2.

Pienso en qué condiciones se tienen que dar, y busco la forma (con las herramientas que tengo) de expresarlo en Prolog.

  
Cuando termino, pongo el punto al final de la última cláusula ... y ya está, *no-hay-nada-más-que-hacer*.

P.ej. ¿cuándo es cierto que un tour T pasa por un país P? Cuando la lista de ciudades de T incluye alguna del país P.

Hay muchas veces en que necesito un individuo que no es un argumento del predicado. En ese caso, pienso qué condiciones la relacionan con los individuos que ya tengo (argumentos + los que traje en condiciones anteriores en la misma cláusula). Típicamente, voy a designar estos nuevos individuos con variables, o con cosas más complejas si me conviene aplicar pattern matching.

Veamos algunos ejemplos:

  
Tenemos que hacer el predicado paisInteresante/1, un país es interesante si tiene al menos dos ciudades grosas.

En este caso necesito dos ciudades del país, las voy a llamar C1 y C2.

¿Qué condiciones tienen que cumplir C1 y C2?

-   Ser del país, predicado pais/2 que relaciona ciudad con país (¡¡en ese orden!!). El país lo tengo, es el argumento de paisInteresante
-   Ser grosas, predicado ciudadGrosa/1

Queda

`   paisInteresante(P):- ciudad(C1,P), ciudad(C2,P), ciudadGrosa(C1), ciudadGrosa(C2).`

  
Resolvamos pasaPorPais/2 ... acá necesitamos

-   la lista de ciudades, para eso tenemos el predicado tour/2, la voy a llamar LCiud.
-   la ciudad, a la que voy a llamar C. ¿Qué le tiene que pasar a C? Estar en LCiud (member/2) y ser del país (pais/2)

Queda

`   pasaPorPais(Tour,Pais):- tour(Tour,LCiud), member(C,LCiud), pais(C,Pais).`
