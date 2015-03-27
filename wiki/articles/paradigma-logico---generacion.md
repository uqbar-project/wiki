El concepto de "generación" puede servir para acotar el conjunto de valores posibles para una variable, pero también para ligar la variable con un valor, ya que eso cambia la forma en la que se evalúan las siguientes [consultas](paradigma-logico---un-poco-de-nomenclatura-consultas.html).

Esto último tiene que ver con los [problemas de inversibilidad](paradigma-logico---casos-de-no-inversibilidad.html), y los casos de uso se dividen en dos grandes grupos:

a) Cuando la generación posibilita la siguiente consulta, por ejemplo:

edad(Persona, E), E &gt; 18.

Necesito que E tenga un valor para poder evaluar E&gt;18, si no se rompe.

Ese sería el caso fácil, si rompe es un claro indicador de que el código no funciona para hacer esa clase de consultas, es fácil de identificar el problema.

b) Cuando la semántica de la consulta es distinta a la que quiero, por ejemplo para la siguiente consulta:

`      findall(CantEjercitos,ocupa(Jugador,_,CantEjercitos),ListCants),`

- Si la variable Jugador llega ligada, podría leerse como: "Dado este Jugador, traeme todas las cantidades de ejércitos en cada país que ocupa". - En cambio, si jugador está libre, se debería leer como: "Traeme todas las cantidades de ejércitos de todos los países ocupados por CUALQUIER JUGADOR".

Entonces, no es que sea obligatorio generar, sino que el significado de la consulta es totalmente distinta, y dependiendo de lo que querramos hacer, hay que ligar previamente el jugador o no. Si nuestra intención es definir un predicado cantidadEjercitos/2 que relaciona un jugador con la cantidad total de ejércitos que tiene y lo hacemos de la siguiente forma:

`   cantidadEjercitos(Jugador,Cant):-`
`      findall(N,ocupa(Jugador,_,N),ListCants),`
`      sumlist(ListCants,Cant).`

Este predicado va a funcionar correctamente (dada una base de conocimientos con muchos jugadores que ocupan países), si la consulta realizada es individual respecto al jugador, pero no es totalmente inversible. Para que lo sea, tenemos que generar al jugador.

`   cantidadEjercitos(Jugador,Cant):-`
`      jugador(Jugador),`
`      findall(N,ocupa(Jugador,_,N),ListCants),`
`      sumlist(ListCants,Cant).`
