En contraposición a los átomos y los números, que son individuos simples, existen los individuos compuestos, estos son:

-   los [functores](paradigma-logico---functores.html)
-   las [listas](paradigma-logico---listas.html)

Para acceder a los componentes de un individuo compuesto podemos usar Pattern Matching.

En el caso de los functores, los componentes son los argumentos del functor. Si los quiero por separado, p.ej. si tenemos un functor de la forma canilla(Forma,Material,Color) y tenemos un predicado vende/2 que relaciona un local con una cosa que vende:

`   vende(pepe,canilla(triangular,hierro,azul)).   `

podemos armar este predicado

`   hayMaterial(Local,Mat):- vende(Local,canilla(_,Mat,_)).`

en donde estamos abriendo el functor canilla/3 mediante pattern matching y así unificando la variable Mat con el material de la canilla que se vende en el local.

En el caso de las listas hay dos patrones básicos, una lista puede ser la lista vacía o con almenos un elemento. En caso de no ser la lista vacía los componentes son cabeza y cola, y la notación es

`  [Cabeza | Cola]`

como la cola también es una lista, si la lista con la que quiero matchear espero que tenga al menos dos elementos puedo hacer:

`  [Elem1 | [ Elem2 | Resto ] ]`

Probar p.ej. con esta consulta

`  ?- [Elem1 | [ Elem2 | Resto ] ] = [a,e,i,o,u].`

Alternativamente se puede usar el patrón `[Elem1,` `Elem2` `|` `Resto]` con el mismo objetivo, es un azúcar sintáctico para unificar dos variables con los primeros dos elementos de la lista.

El caso típico para usar Pattern Matching sobre listas es en la [recursividad](recursividad-en-logico-recursividad-con-listas.html).

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
