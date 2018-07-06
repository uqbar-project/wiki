---
layout: article
title: Recursividad en logico
---

Un predicado recursivo es aquel que en alguna de sus cláusulas se invoca a sí mismo. Los predicados recursivos para poder funcionar correctamente necesitan contar con algún caso base que corte la recursividad.

Recursividad Sin Listas
-----------------------

Un primer ejemplo es el predicado `ancestro/2` que relaciona a una persona con otra si la primera cumple con ser su padre o ser ancestro de su padre. Teniendo un predicado `padre(Padre, Hijo)` podemos hacer:

```
ancestro(Ancestro, Descendiente):-
  padre(Ancestro, Descendiente).
  
ancestro(Ancestro, Descendiente):-
  padre(Padre, Descendiente),
  ancestro(Ancestro, Padre).
```

Acá se ve que `ancestro/2` tiene dos cláusulas: una que se invoca al mismo predicado ancestro/2 y otra que no. No importa cuál de las dos definamos primero, porque Prolog va a intentar ambas de todos modos. Lo que sí es importante de la cláusula recursiva es que demos un pasito que nos acerque al caso base antes de hacer la consulta recursiva, en este caso es preguntar quién es ancestro del padre de la persona que nos interesaba en primer lugar.

Algo que puede parecer complejo es analizar la [inversibilidad](paradigma-logico---inversibilidad.html) de este predicado. Teniendo en cuenta que el predicado `padre/2` es inversible, el caso base es inversible, pero ¿qué pasa con el caso recursivo? Las variable Descendiente se va a ligar gracias a `padre/2`, y la variable Ancestro aparece por primera vez al consultar el mismo predicado que estamos definiendo... Bueno, gracias a que el caso base es inversible, podemos confiar que eventualmente se va a ligar, por ende `ancestro/2` se hace inversible a sí mismo.

Otro ejemplo típico es el factorial:

```
factorial(0,1).
factorial(N,F):- 
  Anterior is N-1,
  factorial(Anterior,F2),
  F is F2*N.
```

Esta definición resuelve el problema, pero no hay que olvidarse que en el paradigma lógico la búsqueda de soluciones es exhaustiva. Si consultamos por el factorial de 1, la primer respuesta será 1 ya que el factorial de 0 también es 1. Pero siendo que 0 también matchea con la variable N entrará en un loop infinito al segundo intento. Por ese motivo, una definición correcta sería:

```
factorial(0,1).
factorial(N,F):- N > 0,
 Anterior is N-1,
 factorial(Anterior,F2),
 F is F2*N.
```

De esa forma, ambos casos son excluyentes entre sí y el 0 sólo puede tener como respuesta al 1.

¿Y la inversibilidad? Algo que salta a la vista es que para consultar el factorial del anterior necesitamos calcular el anterior, y también necesitamos validar que N > 0, y eso impone restricciones porque los predicados > e is necesitan que N esté ligada con un valor concreto. O sea que esta consulta no va a funcionar:

`?- factorial(N, 6).`

Analicemos la inversibilidad para la segunda aridad. ¿Esta otra consulta, funcionaría?

`?- factorial(3, Factorial).`

La variable F aparece recién al final, a la izquierda del is, con lo cual no habría problema por ese lado.

De hecho, si no fuera inversible para la segunda aridad la consulta recursiva no se podría resolver, porque se está usando una variable F2 que no tiene chances de estar ligada previamente como para que sea una consulta individual. Al igual que para el primer ejemplo, que esa consulta recursiva funciona gracias a que tiende al caso base que es inversible.

Recursividad Con Listas
-----------------------

Las [listas](paradigma-logico---listas.html) son indiviuos compuestos cuya naturaleza es recursiva, por ese motivo la recursividad es una forma común para trabajar con ellas. La clave para hacer un ejercicio, si éste se resuelve con recursividad, es poder pensar recursivamente. Ésto significa que cuando digan, por ejemplo "una sumatoria se define como....." ahí, en la definición, tienen que usar el concepto de sumatoria de nuevo, para definirla. ¿Cómo? Y, separando el problema, por ejemplo, en cabeza y cola. Y ese es el chiste, juntando esas dos cosas y pensando un poquito, pueden hacer recursividad sobre listas.

### Sumatoria

La cola de una sumatoria ¿qué tiene que ver con la sumatoria de la lista entera? Y, la sumatoria de la cola es casi todo el problema resuelto, sólo le falta agregar un detallito que implica trabajar con el elemento que tenemos disponible:
```
sumatoria([Cabeza|Cola], S):-
       sumatoria(Cola,SCola), % Esto ya es casi todo el problema resuelto! Solo falta sumar la cabeza:
       S is SCola + Cabeza.
```

Y por supuesto que necesitamos un caso base. El caso base se piensa generalmente por exclusión.... ¿Qué caso no consideré arriba? La lista vacía. (En éste caso)

`sumatoria([],0).`

### Ultimo

A ver, el último de una lista... ¡Es el mismo último de la cola!:

```
ultimo([Cabeza|Cola],Ultimo):-
          ultimo(Cola,Ultimo).
```

Caso base, una lista con un elemento (la lista vacía no tiene último, sería esperable que una consulta para la lista vacía de falso):

`ultimo([E],E).`

Completito:

```
ultimo([_|Cola],Ultimo):-
          ultimo(Cola,Ultimo).
ultimo([E],E).
```

### Todos menos el último

Ahora, el principio de la lista (todos menos el último). Éste es loquito, porque podemos partir ambas listas, así:

`principio([Cabeza|Cola],[Cabeza|PrincipioDeLaCola]):-`

Y claro, porque la cabeza es la misma en la lista original que en su principio. Pero hay que relacionar Cola y PrincipioDeLaCola.....
```
principio([Cabeza|Cola],[Cabeza|PrincipioDeLaCola]):- 
        principio(Cola,PrincipioDeLaCola).
principio([E],[]).
```

### Reverse

Entonces, pensemos otro problema: dar vuelta una lista. Es un poco más interesante: Necesitamos poner el último adelante, y la cola de la lista dada vuelta es el principio de la lista al revés:

`reverse(ListaOriginal,[Ultimo|PrincipioAlReves]):-`

Uf, pero hay que ver de dónde sale eso! Y, la llamada recursiva la conocemos:

`reverse(PrincipioDeLaLista,PrincipioAlReves),`

Y listo, ahora hay que poner antes y después de esa condición cosas para que ligue las variables correspondientes; Ligamos PrincipioDeLaLista, antes de la llamada recursiva:
```
reverse(ListaOriginal,[Ultimo|PrincipioAlReves]):-
            principio(ListaOriginal,PrincipioDeLaLista),
            reverse(PrincipioDeLaLista,PrincipioAlReves), ...
```

Y ahora nos falta saber de dónde sacamos el último:
```
reverse(ListaOriginal,[Ultimo|PrincipioAlReves]):-
            principio(ListaOriginal,PrincipioDeLaLista),
            reverse(PrincipioDeLaLista,PrincipioAlReves),
            ultimo(ListaOriginal,Ultimo).
```
Les dejo el caso base a ustedes.

### Ejercicio: Subconjunto

¿Cómo se hace el subconjunto de una lista? (sin permutaciones)

Así tiene que funcionar:
```
`?- subconjunto([1,2,3],Sub).`
Sub = [];
Sub = [2];
Sub = [2,3];
Sub = [3];
Sub = [1];
Sub = [1,2];
Sub = [1,2,3];
Sub = [1,3];
false.
```
Pista: El subconjunto puede pensarse con dos casos recursivos más el caso base.
