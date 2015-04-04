El concepto de "generación" puede servir para acotar el conjunto de valores posibles para una variable, pero también para ligar la variable con un valor, ya que eso cambia la forma en la que se evalúan las siguientes [consultas](paradigma-logico---un-poco-de-nomenclatura-consultas.html).

Esto último tiene que ver con los [problemas de inversibilidad](paradigma-logico---casos-de-no-inversibilidad.html), y los casos de uso se dividen en dos grandes grupos:

a) Cuando la generación posibilita la siguiente consulta, por ejemplo:

edad(Persona, E), E &gt; 18.

Necesito que E tenga un valor para poder evaluar E&gt;18, si no se rompe.

Ese sería el caso fácil, si rompe es un claro indicador de que el código no funciona para hacer esa clase de consultas, es fácil de identificar el problema.

b) Cuando la semántica de la consulta es distinta a la que quiero, por ejemplo para la siguiente consulta, asumiendo que tenemos en nuestra base de conocimientos un predicado ocupa/3 que relaciona un jugador, un país y la cantidad de ejércitos que tiene en ese país:

`      not((ocupa(Jugador, Pais, CantEjercitos), CantEjercitos > 3))`

- Si la variable Jugador llega ligada y Pais y CantEjercitos llegan libres, podría leerse como: "Dado este Jugador, no ocupa ningún país con más de 3 ejércitos". Si Pais también llega ligada, la consulta que estamos haciendo sería: "No es cierto que este jugador ocupa este país con más de 3 ejércitos".

- En cambio, si Jugador y País están libres, se debería leer como: "No existe ningún jugador que ocupe algún país con más de 3 ejércitos".

Entonces, no es que sea obligatorio generar, sino que el significado de la consulta es totalmente distinto, y dependiendo de lo que querramos hacer, hay que ligar previamente el jugador y/o el país o no.

Veamos un ejemplo con el mismo dominio usando findall/3. Si nuestra intención es definir un predicado cantidadEjercitos/2 que relaciona un jugador con la cantidad total de ejércitos que tiene en todo el mundo y lo hacemos de la siguiente forma:

`   cantidadEjercitos(Jugador,Cant):-`
`      findall(N,ocupa(Jugador,_,N),ListCants),`
`      sumlist(ListCants,Cant).`

Este predicado va a funcionar correctamente (dada una base de conocimientos con muchos jugadores que ocupan varios países) si la consulta realizada es individual respecto al jugador, pero no es totalmente inversible. Para que lo sea, tenemos que generar al jugador.

`   cantidadEjercitos(Jugador,Cant):-`
`      jugador(Jugador),`
`      findall(N,ocupa(Jugador,_,N),ListCants),`
`      sumlist(ListCants,Cant).`

De esa forma podremos consultar:

` ?- cantidadEjercitos(Jugador, Cantidad).`
