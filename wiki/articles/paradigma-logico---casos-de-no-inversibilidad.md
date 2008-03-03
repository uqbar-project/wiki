Negación: las variables que aparecen en la parte de la cláusula deben llegar ligadas, a menos que sean variables afectadas por un "para ningún", en este caso deben llegar libres.

Aritmética: todas las variables a la derecha del is deben llegar ligadas al is.

Findall
-------

    ganoContra(boca, riber).
    ganoContra(boca, zanLorenso).
    ganoContra(boca, beles).

    ganoContra(riber, zanLorenso).

    ganoContra(beles, riber).

    superEquipo( Equipo ) :- findall(P, ganoContra(Equipo, P), Partidos), length(Partidos, CantGanados), CantGanados > 2.

    Ahora fijate tirando en prolog la siguiente consulta:

    superEquipo( X ).

Forall
------
