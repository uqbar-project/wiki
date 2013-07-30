El polimorfismo permite obtener soluciones más genéricas, que sean válidas para diferentes tipos de datos contemplando las particularidades de cada uno de ellos. En general podemos decir que dos cosas son polimórficas cuando desde algún punto de vista comparten un tipo, o sea que pueden ser tratados indistintamente por quienes no les interesen los detalles en los cuales difieren.

Si bien todos los predicados pueden recibir cualquier tipo de argumento, muchas veces el uso que se hace de ellos en el interior de las cláusulas que lo definen, delimita un rango de tipos de valores que tiene sentido recibir. En lógico no sucede un error si se recibe un dato de tipo diferente a los esperados, sino que directamente la consulta falla por no poder unificar el valor recibido con lo esperado, descartándolo como posible respuesta a la misma.

Ejemplo de Polimorfismo usando functores
----------------------------------------

Si tenemos el siguiente requerimiento: "se tiene 3 tipos de vehiculos autos, camiones y bicicletas. Un auto es viejo si su patente es menor a F, un camion es viejo si tiene más de 60000km o más de 10 años, una bicicleta es vieja si la fecha de fabricacion es de año anterior al 2006" 1) de qué manera puedo tener una lista de diferentes vehiculos en el paradigma lógico 2) si quiero filtrar los vehículos viejos, indicar dónde y cómo se da el polimorfismo entre cada uno de los vehiculos y quién aprovecha ese polimorfismo.

La idea del polimorfismo en este caso es que exista un predicado esViejo ande con autos, camiones y bicicletas y pueda ser usado de forma genérica por otro predicado. En este caso queremos no pensar en el tipo de vehículo al hacer un findall.

Supongamos que nuestra base de conocimiento tiene los siguientes hechos:

`hoy(fecha(22,12,2008)).`
`vehiculo(auto("A2000")).`
`vehiculo(auto("H2342")).`
`vehiculo(camion(12000,2005)).`
`vehiculo(camion(70000,2003)).`
`vehiculo(camiion(30000,1997)).`
`vehiculo(bici(fecha(30,10,2005))).`
`vehiculo(bici(fecha(20,12,2008))).`

Modelar a los vehículos con functores nos permite tener individuos más complejos que si sólo ponemos

`vehiculo(auto,"A2000").`

Además nos permite usar el mismo predicado vehiculo (de aridad 1) en este caso, para todos los tipos de vehículos, lo cual se aprovecha con el camión que tiene año y kilometraje y de otro modo no podría usarse el mismo predicado.

Si lo que queremos es obtener todos los vehículos como se pide en el punto 1 alcanza con consultar:

`findall(Vehiculo,vehiculo(Vehiculo),Vehiculos).`

Ahora, para resolver el requerimiento podemos hacer un predicado esViejo/1 que se verifique si se cumplen las condiciones explicadas anteriormente. Para ello usaremos varias cláusulas que, por medio de pattern matching, se apliquen a cada tipo de vehículo particular.

`esViejo(auto(Patente)):- Patente > "F".`
`esViejo(camion(Kilometraje,_)):- Kilometraje > 60000.`
`esViejo(camion(_,Anio)):- hoy(fecha(_,_,AnioActual)),`
` AnioActual - Anio > 10.`
`esViejo(bici(fecha(_,_,Anio)):- Anio < 2006.`

Con esto podemos filtrar la lista de los vehículos viejos de la siguiente forma:

`findall(Vehiculo,(vehiculo(Vehiculo),esViejo(Vehiculo)),Vehiculos).`

Podemos decir entonces que el predicado polimórfico es esViejo, porque funciona con distintos tipos de vehiculos y quien lo aprovecha es el findall que no necesita hacer distinciones sobre los tipos de vehículos.
