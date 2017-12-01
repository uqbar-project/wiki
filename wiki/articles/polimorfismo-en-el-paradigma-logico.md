---
layout: article
title: Polimorfismo en el paradigma logico
---

El polimorfismo permite obtener soluciones más genéricas, que sean válidas para diferentes tipos de datos contemplando las particularidades de cada uno de ellos. En general podemos decir que dos cosas son polimórficas cuando desde algún punto de vista comparten un tipo, o sea que pueden ser tratados indistintamente por quienes no les interesen los detalles en los cuales difieren.

Si bien todos los predicados pueden recibir cualquier tipo de argumento, muchas veces el uso que se hace de ellos en el interior de las cláusulas que lo definen, delimita un rango de tipos de valores que tiene sentido recibir. En lógico no sucede un error si se recibe un dato de tipo diferente a los esperados, sino que directamente la consulta falla por no poder unificar el valor recibido con lo esperado, descartándolo como posible respuesta a la misma.

Ejemplo de Polimorfismo usando functores
----------------------------------------

Si tenemos el siguiente requerimiento: "se tiene 3 tipos de vehiculos autos, camiones y bicicletas. Un auto es viejo si su patente es menor a F, un camion es viejo si tiene más de 60000km o más de 10 años, una bicicleta es vieja si la fecha de fabricacion es de año anterior al 2006"

Supongamos que nuestra base de conocimiento tiene los siguientes hechos:

```Prolog
hoy(fecha(22,12,2008)).
vehiculo(auto("A2000")).
vehiculo(auto("H2342")).
vehiculo(camion(12000,2005)).
vehiculo(camion(70000,2003)).
vehiculo(camion(30000,1997)).
vehiculo(bici(fecha(30,10,2005))).
vehiculo(bici(fecha(20,12,2008))).
```

Si quiero hacer una consulta que me diga si todos los vehículos de la base de conocimientos son viejos, ¿cómo hago para manejar el tema de que los datos tienen formas distintas y la lógica asociada a las distintas formas varía?

La alternativa infeliz no polimórfica sería hacer algo así:

```Prolog
autoViejo(Patente):- Patente > "F".
camionViejo(Kilometraje,_):- Kilometraje > 60000.
camionViejo(_,Anio):- hoy(fecha(_,_,AnioActual)),
                      AnioActual - Anio > 10.
biciVieja(fecha(_,_,Anio)):- Anio < 2006.

?- forall(vehiculo(auto(Patente)), autoViejo(Patente)),
   forall(vehiculo(camion(Kms, Anio)), camionViejo(Kms, Anio)),
   forall(vehiculo(bici(Fecha)), biciVieja(Fecha)).
```

Para que esto se cumpla todos los vehículos deben cumplir la correspondiente validación del forall con lo cual estaría funcionando como necesitamos, pero la solución elegida es bastante molesta y repetitiva. Sin contar que si queremos incorporar las motos a nuestra base de conocimientos, la consulta sería aún más larga.

La idea del polimorfismo nos viene bárbaro en este caso, y lo único que necesitamos es que exista un predicado esViejo/1 ande con autos, camiones y bicicletas y por ende pueda ser usado de forma genérica por otro predicado o consulta sin tener que pensar en el tipo de vehículo.

Ahora, para resolver el requerimiento de forma polimórfica tenemos que hacer el predicado esViejo/1 que se verifique si se cumplen las condiciones explicadas anteriormente. Para ello usaremos varias cláusulas que, por medio de pattern matching, se apliquen a cada tipo de vehículo particular.

```Prolog
esViejo(auto(Patente)):- Patente > "F".
esViejo(camion(Kilometraje,_)):- Kilometraje > 60000.
esViejo(camion(_,Anio)):- hoy(fecha(_,_,AnioActual)),
                          AnioActual - Anio > 10.
esViejo(bici(fecha(_,_,Anio))):- Anio < 2006.
```

Con esto podemos realizar la consulta que queríamos de esta forma:

```Prolog
?- forall(vehiculo(Vehiculo), esViejo(Vehiculo)).
```

Podemos decir entonces que el predicado polimórfico es esViejo/1, porque funciona con distintos tipos de vehiculos, y quien lo aprovecha es la consulta del forall que no necesita hacer distinciones sobre los tipos de vehículos.

Es importante notar que modelar a los vehículos con functores nos permite tener individuos más complejos que si sólo ponemos

```Prolog
vehiculo(auto,"A2000").
```

Y además nos permite usar el mismo predicado (vehiculo/1 en este caso) para todos los tipos de vehículos, lo cual se aprovecha principalmente con el camión que tiene año y kilometraje, y al tener distinta cantidad de valores afectaría directamente a la aridad del predicado (por más que se llamen igual, si la cantidad de parámetros es distinta, es otro predicado, ya que no hay forma de consultarlos a la vez).

## Errores comunes

### Repito lógica por hacer mal uso de polimorfismo

Supongamos que tenemos la siguiente base de conocimiento:

```Prolog
juguete(ingenio(cuboRubik,10)).
juguete(ingenio(encajar,2)).
juguete(munieco(50)).
juguete(peluche(300)).
juguete(peluche(150)).
```

Suponiendo que quiero saber si un juguete es caro, lo que sucede cuando su precio supera los 100 pesos.

Se sabe que el precio de los juguetes de ingenio es de $20 por cada punto de dificultad, el precio del muñeco es el indicado, y el precio de los peluches es la mitad de su tamaño en mm.

Una forma de resolverlo sería:

```Prolog
esCaro(ingenio(_,Dificultad)):-
     100 < Dificultad * 20.
esCaro(munieco(Precio)):- 
     Precio > 100.
esCaro(peluche(Tamanio)):-
     Tamanio / 2 > 100.
```

El problema es que repito la lógica de cuándo un juguete es caro en las tres cláusulas, porque **me falta abstraer la lógica del precio**.

Si tengo la lógica del precio delegada en otro predicado, luego no necesito repetir la lógica de esCaro en cada cláusula:

```Prolog
esCaro(Juguete):-
     precio(Juguete,Precio),
     Precio > 100.
     
precio(ingenio(_,Dificultad), Precio):-
     Precio is Dificultad * 20.
precio(munieco(Precio),Precio).
precio(peluche(Tamanio),Precio):-
     Precio is Tamanio / 2.
```

### Intento poner una variable en el nombre del functor

Supongamos que tenemos la siguiente base de conocimiento:

```Prolog
vehiculo(opp564, camion(mercedes,2014)).
vehiculo(agt445, auto(504,1995)).
vehiculo(mmr444, camion(scania,2010)).
```

Lo siguiente es **incorrecto**:

```Prolog
esViejo(Patente):-
   vehiculo(Patente,Tipo(_,Anio)),
   Anio < 2000.
```

¿Por qué es incorrecto? Primero, Prolog **no me deja usar el nombre del functor como un dato**, que yo pueda poner en una variable ú operar. Además, lo siguiente no sería posible:

```Prolog
vehiculo(ppt666, moto(2010)). % No respeta la aridad
vehiculo(ert434, lancha(2017,yamaha)). % El orden es otro
vehiculo(dfg345, karting(rojo)). % ¡Y puede ser que no incluya la información del año y haya que hacer otra cosa!
```

La siguiente solución sería la correcta, porque arregla todos los problemas mencionados arriba:

```Prolog
esViejo(Patente):-
   vehiculo(Patente,Vehiculo),
   anio(Vehiculo,Anio),
   Anio < 2000.
   
anio(camion(_,Anio),Anio).
anio(auto(_,Anio),Anio).
anio(moto(Anio),Anio).
anio(lancha(Anio,_),Anio).
anio(karting(Color),Anio):- colorDelAnio(Color,Anio).

colorDelAnio(rojo,2010).
colorDelAnio(verde,1990).
colorDelAnio(azul,2015).
```

De esta forma puedo meter **los functores con la forma que yo quiera** , y aún así mi predicado `esViejo` **no cambia**. Esa es la gran ventaja del polimorfismo. Lo único que tengo que hacer es definir el predicado `anio` para ese nuevo tipo de functor, y ya todo anda sin modificar código existente.

```Prolog
esViejo(Patente):-
   vehiculo(Patente,Vehiculo),
   anio(Vehiculo,Anio),
   Anio < 2000.
```
