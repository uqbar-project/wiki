Un predicado recursivo es aquel que en alguna de sus cláusulas se invoca a sí mismo. Los predicados recursivos para poder funcionar correctamente necesitan contar con algún caso base que corte la recursividad.

Recursividad Sin Listas
-----------------------

Un ejemplo típico es el factorial:

`factorial(0,1).`
`factorial(N,F):- Anterior is N-1,`
` factorial(Anterior,F2),`
` F is F2*N.`

Esta definición resuelve el problema, pero no hay que olvidarse que en el paradigma lógico la búsqueda de soluciones es exhaustiva. Si consultamos por el factorial de 1, la primer respuesta será 1 ya que el factorial de 0 también es 1. Pero siendo que 0 también matchea con la variable N entrará en un loop infinito al segundo intento. Por ese motivo, una definición correcta sería:

`factorial(0,1).`
`factorial(N,F):- N > 0,`
` Anterior is N-1,`
` factorial(Anterior,F2),`
` F is F2*N.`

De esa forma, ambos casos son excluyentes entre sí y el 0 sólo puede tener como respuesta al 1.

Recursividad Con Listas
-----------------------

La clave para hacer un ejercicio, si éste se resuelve con recursividad, es poder pensar recursivamente. Ésto significa que cuando digan, por ejemplo "una sumatoria se define como....." ahí, en la definición, tienen que usar el concepto de sumatoria de nuevo, para definirla. ¿Cómo? Y, separando el problema, por ejemplo, en cabeza y cola. Y ese es el chiste, juntando esas dos cosas y pensando un poquito, pueden hacer recursividad sobre listas.

### Sumatoria

La cola de una sumatoria ¿qué tiene que ver con la sumatoria de la lista entera? Y, la sumatoria de la cola es casi todo el problema resuelto, sólo le falta agregar una boludez:

`sumatoria([Cabeza|Cola], S):-`
`       sumatoria(Cola,SCola), % Esto ya es casi todo el problema resuelto! Solo falta sumar la cabeza:`
`       S is SCola + Cabeza.`

Y por supuesto que necesitamos un caso base. El caso base se piensa generalmente por exclusión.... ¿Qué caso no consideré arriba? La lista vacía. (En éste caso)

`sumatoria([],0).`

### Ultimo

A ver, el último de una lista... ¡Es el mismo último de la cola!:

`ultimo([Cabeza|Cola],Ultimo):-`
`          ultimo(Cola,Ultimo).`

Caso base, una lista con un elemento (la lista vacía no tiene último):

`ultimo([E],E).`

Completito:

`ultimo([_|Cola],Ultimo):-`
`          ultimo(Cola,Ultimo).`
`ultimo([E],E).`

### Todos menos el último

Ahora, el principio de la lista (todos menos el último). Éste es loquito, porque podemos partir ambas listas, así:

`principio([Cabeza|Cola],[Cabeza|PrincipioDeLaCola]):-`

Y claro, porque la cabeza es la misma en la lista original que en su principio. Pero hay que relacionar Cola y PrincipioDeLaCola.....

`principio([Cabeza|Cola],[Cabeza|PrincipioDeLaCola]):- `
`        principio(Cola,PrincipioDeLaCola). %Una boludez. Caso base:`
`principio([E],[]).`

Y eso fue recursividad para Nintendo64.

### Reverse

Entonces, pensemos otro problema: dar vuelta una lista. Es un poco más interesante: Necesitamos poner el último adelante, y la cola de la lista dada vuelta es el principio de la lista al revés:

`reverse(ListaOriginal,[Ultimo|PrincipioAlReves]):-`

Uf, pero hay que ver de dónde sale eso! Y, la llamada recursiva la conocemos:

`         reverse(PrincipioDeLaLista,PrincipioAlReves),`

Y listo, ahora hay que poner antes y después de esa condición cosas para que ligue las variables correspondientes; Ligamos PrincipioDeLaLista, antes de la llamada recursiva:

`reverse(ListaOriginal,[Ultimo|PrincipioAlReves]):-`
`            principio(ListaOriginal,PrincipioDeLaLista),`
`            reverse(PrincipioDeLaLista,PrincipioAlReves),`

Y ahora nos falta saber de dónde sacamos el último:

`reverse(ListaOriginal,[Ultimo|PrincipioAlReves]):-`
`            principio(ListaOriginal,PrincipioDeLaLista),`
`            reverse(PrincipioDeLaLista,PrincipioAlReves),`
`            ultimo(ListaOriginal,Ultimo).`

Les dejo el caso base a ustedes.

### Ejercicio: Subconjunto

¿Cómo se hace el subconjunto de una lista? (sin permutaciones)

Así tiene que funcionar:

`?- subconjunto([1,2,3],Sub).`
`Sub = [];`
`Sub = [2];`
`Sub = [2,3];`
`Sub = [3];`
`Sub = [`**`1`**`];`
`Sub = [`**`1`**`,2];`
`Sub = [`**`1`**`,2,3];`
`Sub = [`**`1`**`,3];`
`false.`

Pista: El subconjunto puede pensarse con dos casos recursivos más el caso base.
