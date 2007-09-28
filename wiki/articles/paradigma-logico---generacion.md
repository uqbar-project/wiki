El concepto de "generación" puede servir para acotar el conjunto de valores posibles para una variable, pero también al ligar la variable con un valor eso cambia la forma en la que se evalúan las siguientes consultas. Eso tiene que ver con la inversibilidad y los casos de uso se dividen en dos grandes grupos:

a) Donde la generación posibilita la siguiente consulta, por ejemplo:

edad(Persona, E), E &gt; 18.

Necesito que E tenga un valor para poder evaluar E&gt;18, si no se rompe.

Ese sería el caso fácil.

b) En este caso, vos querés que la variable Jugador esté ligada antes del findall, porque la semántica sería distinta. Por ejemplo, la consulta:

`      findall(N,ocupa(Jugador,_,N),ListCants),`

- Si el Jugador está ligado, podría leerse como: "Dado este Jugador, traeme todas las cantidades de ejercitos en cada país que ocupa". - En cambio, si jugador está libre, se debería leer como: "Traeme todas las cantidades de ejercitos de todos los paises ocupados por CUALQUIER JUGADOR".

Entonces, no es que sea obligatorio generar, sino que significa distintas cosas la consulta si vos tenés el findall ligado o no. Probá hacer la consulta "cantidadEjercitos(X, Y)" sobre el predicado con y sin la generación y vas a ver cuáles son las respuestas.

`   cantidadEjercitos(Jugador,Cant):-`
`      jugador(Jugador),`
`      findall(N,ocupa(Jugador,_,N),ListCants),`
`      sumlist(ListCants,Cant). `

`   cantidadEjercitos(Jugador,Cant):-`
`      findall(N,ocupa(Jugador,_,N),ListCants),`
`      sumlist(ListCants,Cant).`
