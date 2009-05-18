Los casos de no inversibilidad, y algunos de no funcionamiento o respuestas incorrectas, están relacionados con variables que no están ligadas en cierto punto (recordando que el análisis debe hacerse "de izquierda a derecha") y deben estarlo, o (menos probable) al revés, variables que deben llegar a cierto punto sin ligar y están ligadas.

Veamos varios casos, que incluyen todos los casos de no-inversibilidad que vemos en la materia.

Negación
--------

Negación: las variables que aparecen en la parte de la cláusula deben llegar ligadas, a menos que sean variables afectadas por un "para ningún", en este caso deben llegar libres. P.ej. esta definición del predicado esPlantaComestible

` esPlantaComestible(Planta):- not(esVenenosa(Planta)).`

no es inversible, porque Planta debe estar ligada antes de llegar al `not`.
¿Por qué? Ver [Paradigma Lógico - negación](paradigma-logico---negacion.html) Para hacer al predicado inversible, generamos el dominio de la variable Planta

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

Miremos esta definición de plantasMismaEspecieDe

` plantasMismaEspecieDe(Planta, ListaPlantasFamiliares):- `
`    findall(P2, mismaEspecie(Planta,P2), ListaPlantasFamiliares).`

y supongamos esta consulta

` ?- plantasMismaEspecieDe(Planta, Plantas).`

Observemos

plantasMismaEspecieDe(Planta, ListaPlantasFamiliares):-

`  esPlanta(Planta), findall(P2, mismaEspecie(Planta,P2), ListaPlantasFamiliares).`

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

esEspecieComestible(Especie):- esEspecie(Especie), forall(especieDe(Planta,Especie), esPlantaComestible(Planta)).
