1.

El forall es un predicado, o sea, no conviene pensar qué "hace", sino qué relaciona, y/o cuándo se verifica. El predicado forall conviene pensarlo por lo segundo, o sea cuándo se verifica.

2.

El forall recibe dos parámetros, los dos pueden verse como consultas. Las consultas se hacen sobre predicados, o sea que forall es un predicado de orden superior, porque puede manejar predicados entro de sus parámetros.

Hagámosnos la pregunta ... ¿cuándo se verifica?

Cuando todas las respuestas de la primer consulta son respuestas de la segunda. Dicho "en sencillo", cuando a todos los que les pasa lo primero, les pasa lo segundo.

Entonces, en las situaciones donde decimos "a todos los que ...blah... les pasa ...bleh..." es probable que el forall nos venga bien.

3.

Un ejemplo: partimos de estos hechos

    dulce(chocolate).
    dulce(caramelo).
    dulce(durazno).
    amargo(radicheta).
    amargo(cebada).

    leGusta(roque,chocolate).
    % ... y muchos hechos más que describen los gustos de un grupo de personas

y queremos definir esTierno/1, donde decimos que una persona es tierna si todas las cosas que le gustan son dulces.

Estamos en un caso candidato a forall: "a todas las cosas que le gustan les tiene que pasar ser dulces". En Prolog:

`   esTierno(P):- forall(leGusta(P,Cosa), dulce(Cosa)).`

Contar el tema de las variables, es bastante loco y un poquito en contra de lo que decimos
