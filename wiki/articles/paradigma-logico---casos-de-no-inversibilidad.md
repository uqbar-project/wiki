Intrroducción
-------------

Los casos de no inversibilidad, y algunos de no funcionamiento o respuestas incorrectas, están relacionados con variables que no están ligadas en cierto punto (recordando que el análisis debe hacerse "de izquierda a derecha") y deben estarlo, o (menos probable) al revés, variables que deben llegar a cierto punto sin ligar y están ligadas.

Veamos varios casos, que incluyen todos los casos de no-inversibilidad que vemos en la materia.

Negación
--------

Negación: las variables que aparecen en la parte de la cláusula deben llegar ligadas, a menos que sean variables afectadas por un "para ningún", en este caso deben llegar libres. P.ej. esta definición del predicado esPlantaComestible

` esPlantaComestible(Planta):- not(esVenenosa(Planta)).`

no es inversible, porque Planta debe estar ligada antes de llegar al `not`. ¿Por qué? Ver [Paradigma Lógico - negación](paradigma-logico---negacion.html)
Para hacer al predicado inversible, generamos el dominio de la variable Planta

` esPlantaComestible(Planta):- esPlanta(Planta), not(esVenenosa(Planta)).`

Aritmética
----------

Aritmética: todas las variables a la derecha del is deben llegar ligadas al is. P.ej. el predicado precioPorCantidad definido así

` precioPorCantidad(Planta,Cantidad,PrecioTotal):- `
`       precioPlanta(Planta,Precio), PrecioTotal is Cantidad * Precio.`

no es inversible para el 2do argumento, porque si no se indica un número para Cantidad en la consulta, al llegar a la cuenta hay una variable no ligada a la derecha del `is`.
En este caso no hay forma trivial de arreglarlo para que el predicado sea totalmente inversible.

En este caso

` importeCompra(Persona, ImporteTotal):- `
`       ImporteTotal is Precio*1.21, compra(Persona, Planta), precioPlanta(Planta,Precio).`

ninguna consulta que involucre a esta cláusula va a andar porque no hay forma de que la variable Precio esté ligada al momento de hacer la cuenta. Arreglar este caso es fácil: ponemos el `is` al lado del punto

` importeCompra(Persona, ImporteTotal):- `
`       compra(Persona, Planta), precioPlanta(Planta,Precio), ImporteTotal is Precio*1.21.`

Comparación
-----------

Si se compara por mayor, menor, mayor o igual, menor o igual, distinto, lo que comparemos debe estar ligado. P.ej. esta definición

` plantaHeavy(Planta):-Nivel > 5, nivelVeneno(Planta,Nivel).`

es incorrecta, porque Nivel no está ligado al momento de comparar. Esta forma

` plantaHeavy(Planta):-nivelVeneno(Planta,Nivel),Nivel > 5.`

sí es correcta.

Findall
-------

Miremos esta definición de plantasDerivadasDe

` plantasDerivadasDe(Planta, ListaPlantasFamiliares):- `
`    findall(P2, derivadaDe(Planta,P2), ListaPlantasFamiliares).`

y supongamos esta consulta

` ?- plantasDerivadasDe(Pl, Plantas).`

Miremos fijos el findall, recordando que unifica el 3er argumento con la lista de la parte indicada en el 1er argumento de todas las respuestas a la consulta del 2do argumento.
En este caso: va a ligar `ListaPlantasFamiliares` con la lista de los `P2` para cada respuesta a la consulta `derivaDe(Planta,P2)`.
Como en la consulta no se liga `Planta`, entonces las respuestas a `derivaDe(Planta,P2)` van a ser **todos** los pares de plantas (planta,derivada), y por lo tanto los `P2` van a ser **todas** las plantas derivadas de alguna planta.
P.ej. si tenemos

` derivaDe(p1,p3).`
` derivaDe(p2,p4).`
` derivaDe(p2,p5).`

`ListaPlantasFamiliares` va a ser `[p2,p4,p5]`. Esto nos muestra que con esta definición el predicado no es inversible para el primer argumento, porque las listas de "derivadas de la misma planta" son \[p3\] por un lado, y \[p4,p5\] por otro; \[p2,p4,p5\] no puede ser un 2do argumento correcto para este predicado.

Para que que el predicado sea totalmente inversible debemos asegurar que la variable `Planta` *entra ligada al findall*.

` plantasDerivadasDe(Planta, ListaPlantasFamiliares):- `
`    esPlanta(Planta), `
`    findall(P2, derivaDe(Planta,P2), ListaPlantasFamiliares).`

Miremos ahora qué pasa si definimos el predicado así

` plantasDerivadasDe(Planta, ListaPlantasFamiliares):- `
`    esPlanta(Planta), derivaDe(Planta,P2), `
`    findall(P2, derivaDe(Planta,P2), ListaPlantasFamiliares).`

En este caso, al llegar al findall tanto Planta como P2 ya están ligadas, entonces la consulta `derivaDe(Planta,P2)` puede tener a lo sumo una respuesta (positiva, la respuesta "no" no se cuenta), entonces ListaPlantasFamiliares va a tener a lo sumo un elemento. Vamos a obtener respuestas incorrectas, p.ej.

` ?- plantasDerivadasDe(p2, Lista).`
` Lista = [p4]`

porque la variable P2, que debe llegar sin ligar al findall, llega ligada.

**Un ejemplo futbolero**

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

Esta definición del predicado esEspecieComestible

` esEspecieComestible(Especie):- `
`     forall(especieDe(Planta,Especie), esPlantaComestible(Planta)).`

no es inversible, porque Especie debe llegar ligado al forall. Si no llega ligada, para que el forall se verifique deben ser comestibles **todas** las plantas que sean de alguna especie. Esto está explicado en detalle en [el artículo sobre forall](paradigma-logico---el-forall-forall-e-inversibilidad.html).

Para que sea inversible debemos generar el dominio para la variable Especie

` esEspecieComestible(Especie):- `
`     esEspecie(Especie),`
`     forall(especieDe(Planta,Especie), esPlantaComestible(Planta)).`

En resumen
----------

Resumimos los cinco casos de inversibilidad con un ejemplo de cada uno

**Negación**  

` esPlantaComestible(Planta):- not(esVenenosa(Planta)).`
` % Planta debe llegar ligada al not`

**Aritmética**  

` precioPorCantidad(Planta,Cantidad,PrecioTotal):- `
`     precioPlanta(Planta,Precio), PrecioTotal is Cantidad * Precio.`
` % Cantidad debe llegar ligada al is`

**Comparación**  

` plantaHeavy(Planta):- Nivel > 5, nivelVeneno(Planta,Nivel).`
` % Nivel debe llegar ligada a la comparación`

**findall**  

` plantasDerivadasDe(Planta, ListaPlantasFamiliares):- `
`     findall(P2, derivadaDe(Planta,P2), ListaPlantasFamiliares).`
` % Planta debe llegar ligada al findall`

**forall**  

` esEspecieComestible(Especie):- `
`     forall(especieDe(Planta,Especie), esPlantaComestible(Planta)).`
` % Especie debe llegar ligada al forall`
