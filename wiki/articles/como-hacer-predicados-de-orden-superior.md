Recordemos los predicados de orden superior que vimos hasta el momento, esos “predicados que relacionan predicados”:

-   not/1
-   findall/3
-   forall/2

¿Eso es todo lo que hay? No, por supuesto que no. Existen muchos otros predicados de orden superior "pre-construidos" (built-in), pero esta base nos alcanza para lo que queremos ver en la materia. Pero lo más interesante es que podemos construir nuestros propios predicados de orden superior, sin embargo vamos a ver que no es tan natural como sí era en Haskell.

call / 1
--------

El predicado call/1 nos permite evaluar un predicado pasado por parámetro. Retomando uno de nuestros primeros ejemplos, veamos cómo se usaría:

`?- call(padre(Padre, Hijo)).`
`Padre = homero`
`Hijo = bart ;`
`...`

Pero eso no aporta mucho respecto de hacerlo en forma directa:

`?- padre(Padre, Hijo).`
`Padre = homero`
`Hijo = bart ;`
`...`

Se supone que sólo tiene sentido usar esto si no sabemos qué consulta es la que nos van a pasar por parámetro y usamos el call/1 para definir algo más genérico. Veamos otra variante, entonces, que puede resultarnos más interesante y útil para lo que sí podemos llegar a usar.

call / \_
---------

El predicado call/\_ también nos permite evaluar un predicado pasado por parámetro, pero separando los parámetros que el mismo recibe:

`?- call(padre, Padre, Hijo).`
`Padre = homero`
`Hijo = bart ;`
`...`

O también:

`?- call(padre(homero), Hijo).`
`Hijo = bart ;`
`...`

Momento, momento... entonces, ¿cuál es la aridad de call/\_? El predicado call/\_ no tiene definida una aridad fija. Puede tener desde 1 (la versión que vimos antes) hasta N + 1, siendo N la aridad del predicado que se recibe como primer parámetro.

Usando este predicado podemos hacer cosas equivalentes (¡no iguales!) a las que hacíamos con orden superior en Haskell. Juguemos un poco con esto: implementemos map y filter.

Creando nuestros propios predicados de orden superior
-----------------------------------------------------

### el viejo y querido filter

La cuestión de los parámetros es igual a la anterior: vamos a necesitar uno más que lo que tenía filter en Haskell para unificar la lista resultante. Hagamos también dos versiones.

**Versión recursiva:**

`filterRecursivo( _ , [] , [] ).`
`filterRecursivo(Pred, [X | Xs], [X | Ys]):-`
`  call(Pred, X),`
`  filterRecursivo(Pred, Xs, Ys).`
`filterRecursivo(Pred, [X | Xs], Ys):-`
`  not(call(Pred, X)),`
`  filterRecursivo(Pred, Xs, Ys).`

**Versión no recursiva:**

`filterNoRecursivo(Pred, ListaOrigen, ListaResultante):-`
`  findall(X, (member(X, ListaOrigen), call(Pred, X)), ListaResultante).`

Ejemplos de consulta:

`?- filterRecursivo(padre(homero), [herbert, lisa, maggie, homero, bart], ListaFiltrada).`
`ListaFiltrada = [lisa, maggie, bart] ;`
`No`
`?- filterNoRecursivo(padre(homero), [herbert, lisa, maggie, homero, bart], ListaFiltrada).`
`ListaFiltrada = [lisa, maggie, bart] ;`
`No`

Buenísimo :D Este predicado también existe como built-in y, como el título lo dice, es **include/3**.

### el viejo y querido map

Empecemos por lo básico... ¿cuántos parámetros tenía la función map?

`> map f lista`

Tenía dos parámetros, una función de transformación “f” y una lista, y la función era aplicable a cada elemento de la lista.

Entonces, ¿cuántos argumentos va a tener nuestra relación maplist? Vamos a tener el predicado de transformación y la lista, por supuesto. Pero también necesitamos un argumento más para unificarlo con la lista resultante del mapeo. Tenemos también que considerar las cosas que relaciona el predicado: un elemento de la lista original con uno de la lista resultante.

`?- map(Predicado, ListaOriginal, ListaResultante).`

Ejemplo de uso:

`?- map(padre, [homero,abe], Hijos).`
`Hijos = [[bart,lisa,maggie],[homero,herbert]]`

Ok, pensemos cómo lo podríamos implementar para que haga lo que queremos?

**Versión 1 con recursividad**

`mapRecursivo( _ , [] , [] ).`
`mapRecursivo(Pred, [X|Xs], [Y|Ys]):-`
`  call(Pred, X, Y),`
`  mapRecursivo(Pred, Xs, Ys).`

Ejemplos de consulta:

`?- mapRecursivo(padre, [homero,abe], Hijos).`
`Hijos= [bart, homero] ;`
`Hijos= [bart, herbert] ;`
`Hijos= [lisa, homero] ;`
`Hijos= [lisa, herbert] ;`
`Hijos= [maggie, homero] ;`
`Hijos= [maggie, herbert] ;`
`No`

Ok... esto no hace lo que queremos, vemos que vamos a tener múltiples respuestas donde para cada padre me mappea con un único hijo de ese padre. Pensándolo desde un punto de vista genérico, más allá del dominio particular, nuestra primer implementación nos da N respuestas con todas las combinaciones posibles de mapeo, pero siempre con mapeos 1 a 1 para cada elemento de la lista original. Qué más se nos ocurre?

**Versión 2 con findall** También podríamos hacer una versión no recursiva:

`mapNoRecursivo(Pred, ListaOriginal, ListaResultante):-`
`   findall(Y, (member(X, ListaOriginal), call(Pred, X, Y)), ListaResultante).`

Ejemplos de consulta:

`?- mapNoRecursivo(padre, [homero,abe], Hijos).`
`Hijos = [bart, lisa, maggie, homero, herbert] ;`
`No`

Para nuestra implementación no recursiva la respuesta es única, pero están en la misma lista los hijos de todos y según nuestro ejemplo de uso esperado que dijimos al principio queríamos que nos de una lista de listas, sino no podemos distinguir los que son hijos de homero respecto a los de abe.

Las dos implementaciones que realizamos sin embargo funcionarían correctamente con relaciones que cumplan con unicidad (con lo cual estaríamos más cerca del mundo funcional, no siempre queremos esto, depende del problema). Si modeláramos la relación hijosDe/2 como:

`hijosDe(Padre,Hijos):- findall(Hijo, padre(Padre,Hijo), Hijos).`

Podríamos hacer la siguiente consulta

`?- mapNoRecursivo(hijosDe, [homero,abe], Hijos).`
`Hijos = [[bart,lisa,maggie],[homero,herbert]]`

Y el resultado sería idéntico al de nuestro mapRecursivo.

**Y cuál es el que está bien?** La respuesta es depende. Si buscamos múltiples respuestas con relaciones 1 a 1 vamos a querer la primera, si buscamos una única respuesta que englobe todas las respuestas posibles vamos a querer la segunda, y si queremos que sea 1 a 1 con una única respuesta tenemos que asegurarnos de que nuestro dominio esté modelado de forma acorde, incluso podríamos tener una tercer versión como esta:

**Versión 3 con recursividad y findall**

`mapListaDeListas( _ , [] , [] ).`
`mapListaDeListas(Pred, [X|Xs], [Y|Ys]):-`
`  findall(Z,call(Pred, X, Z),Y),`
`  mapListaDeListas(Pred, Xs, Ys).`

Esta consulta va a hacer lo que queríamos originalmente, pero siempre nos va a dar una lista de listas y capaz no queremos eso...

`?- mapListaDeListas(padre, [homero,abe], Hijos).`
`Hijos = [[bart,lisa,maggie],[homero,herbert]]`

Ya que estamos en lógico, pensemos qué otras consultas podríamos querer hacer:

`?- mapRecursivo(padre, Padres, [bart, lisa, maggie]).`
`Padres = [homero, homero, homero] ;`
`No`

Nuestra primer versión recursiva es inversible para el segundo o el tercer argumento (aunque no ambos simultáneamente). Si probamos lo mismo con nuestra versión no recursiva, nos vamos a encontrar con un problema ya que en la implementación estamos usando member/2 con la primera lista, y member/2 no es inversible para la lista.

Lógicamente ninguna versión será inversible respecto al primer argumento, ya que necesita saber qué predicado quiere consultar en el call.

Bueno, la versión built-in de map en SWI-Prolog es **maplist/3**, y se comporta como nuestra versión recursiva.

### ¿Cómo seguimos?

Otro predicado de orden superior muy útil es mejorSegun/3 que relaciona un predicado de aridad 2 a invocar, una lista y al valor que maximiza el segundo argumento del predicado en cuestión. Se animan a resolverlo?
