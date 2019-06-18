---
layout: article
title: Paradigma logico   multiples respuestas
---

Al hacer una [consulta existencial](paradigma-logico---un-poco-de-nomenclatura.html) se puede obtener más de una respuesta. Esto se debe a que estamos trabajando con **relaciones**, no con [funciones](concepto-de-funcion.html). Las funciones son un caso particular de relación que se caracterizan por cumplir con unicidad (para cada valor de entrada hay una única salida), mientras que las relaciones no tienen esa restricción.

P.ej. si Pedro tiene como primos a Lucía, Alan y Guido; entonces la relación "ser primos" relaciona a Pedro con tres personas distintas: 
```prolog
primo(pedro,lucia).
primo(pedro,alan).
primo(pedro,guido).
```

Entonces si consulto quién es un posible primo de Pedro, lo que debe pasar es que se obtengan tres respuestas independientes entre ellas: una respuesta por cada primo que tenga Pedro.
```prolog
?- primo(pedro,Primo).
   Primo = lucia;
   Primo = alan;
   Primo = guido
```

En funcional no tendría sentido que para la función primo existan 3 respuestas para el valor Pedro, dado que no cumpliría con la propiedad de unicidad de las funciones. La única forma de responder que tanto Lucía, Alan y Guido son primos de Pedro es mediante el uso de valores compuestos para representar al conjunto que incluye a todos ellos.

Ahora, si quisiéramos resolver un problema donde esperamos que cada individuo se relacione con un único valor, eso debe asegurarse dentro de la lógica del predicado.

Pongamos un ejemplo sencillo para analizar: supongamos que queremos definir un predicado que relacione a un valor consigo mismo, salvo que ese valor sea el 0, en cuyo caso se lo debe relacionar con el número 1. No alcanzaría sólo con explicitar mediante un hecho que el 0 se relaciona con el 1 y luego generalizar lo que pasa con los otros valores de esta forma de esta forma:
```prolog
p(0, 1).
p(X, X).
```
Ya que si consultamos:
```prolog
?- p(0, Valor).
Valor = 1;
Valor = 0
```
Para que la única respuesta posible sea 1, tenemos que asegurar que el caso general excluya al 0, lo cual puede lograrse de la siguiente forma:
```prolog
p(0, 1).
p(X, X) := X \= 0.
```
