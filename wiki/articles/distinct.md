---
layout: article
title: Paradigma logico,  Predicado distinct
---

Motivación
----------

¿Qué pasa cuando un predicado encuentra **la misma respuesta por varios caminos**?

Si tenés:
```Prolog
?- esPicaro(Quien).
Quien=pepe,
Quien=pepe,
Quien=juan,
Quien=pepe,
Quien=juan.
```

¡Esto está **bien**! No importa por cuántos caminos llegue a la misma persona, lo que importa es que cuando unifique unifique con los que son pícaros.

El problema es cuando querés encontrar **cuántos pícaros son**, es decir, cuando querés _agregar los datos_. Por ejemplo harías:

```Prolog
findall(Picaro,esPicaro(Picaro),Picaros),
length(Picaros,CuantosPicaros).
```

Y querés que te dé 2 (pepe y juan), no 5. 

Bueno, en ese caso, podés utilizar el predicado de orden superior `distinct/2`, de esta forma:
```Prolog
?- distinct(Quien,esPicaro(Quien)).
Quien=pepe,
Quien=juan.
```

Y entonces te queda así:
```Prolog
findall(Picaro,distinct(Picaro,esPicaro(Picaro)),Picaros),
length(Picaros,CuantosPicaros).
```
