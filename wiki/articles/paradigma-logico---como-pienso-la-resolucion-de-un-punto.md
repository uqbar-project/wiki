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

Qué **no** hago
---------------

Pensar en recorrer, no hay que recorrer nada.

Qué **sí** hago
---------------

Si tengo que crear un predicado nuevo, pienso en la aridad, y qué representa cada argumento. P.ej. el predicado pasaPorPais relaciona un tour con un país.

Pienso en qué condiciones se tienen que dar, y busco la forma (con las herramientas que tengo) de expresarlo en Prolog. Cuando termino, pongo el punto al final de la última cláusula ... y ya está, *no-hay-nada-más-que-hacer*. P.ej. ¿cuándo es cierto que un tour T pasa por un país P? Cuando la lista de ciudades de T incluye alguna del país P.

Cuando
