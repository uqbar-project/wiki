Recordar que los individuos compuestos son

-   los functores
-   las listas

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
