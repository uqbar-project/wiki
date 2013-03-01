Recordar que los individuos compuestos son

-   los \[\[Paradigma Lógico - functores|functores\]
-   las \[\[Paradigma Lógico - Listas|listas\]

y que para acceder a los componentes de un individuo compuesto podemos usar pattern matching.

En el caso de los functores, los componentes son los argumentos del functor. Si los quiero por separado, p.ej. si tenemos un functor

`   % canilla(Forma,Material,Color)`
`   % p.ej. tenemos un predicado vende(Local,CosaQueVende)`
`   vende(pepe,canilla(triangular,hierro,azul)).   `

podemos armar este predicado

`   hayMaterial(Local,Mat):- vende(Local,canilla(_,Mat,_)).`

en donde estamos accediendo por pattren

En el caso de las listas, los componentes son cabeza y cola, y la notación es

`  [Cabeza | Cola]`

donde, como la cola también es una lista, puedo hacer

`  [Elem1 | [ Elem2 | Resto ] ]`

Probar p.ej. con esta consulta

`  ?- [Elem1 | [ Elem2 | Resto ] ] = [a,e,i,o,u].`

Combinando constructores de individuos compuestos
-------------------------------------------------

Se pueden combinar estas dos formas de armar individuos compuestos, p.ej. con esta lista de functores.

`  recorrido(linea19, [estacionTren(once), avenida(rivadavia), calle(salguero), `
`                      calle(guardiaVieja), facultad(utnMedrano), calle(forest), estacionTren(chacarita)]).`

si quiero saber por qué estaciones de tren pasa una línea de colectivos, puedo definirlo así

`  pasaPorEstacion(Linea,Estacion):-`
`     recorrido(Linea, Recorrido), member(estacionTren(Estacion),Recorrido).`

si consulto

`  ?- pasaPorEstacion(linea19,Est).`

voy a obtener como respuestas

`  Est = once;`
`  Est = chacarita;`
`  No`
