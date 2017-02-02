---
layout: article
title: Paradigma logico   negacion
---

El predicado de [orden superior](orden-superior.html) más simple, y a la vez muy utilizado, es el `not/1`, que representa una negación. Si `P` es una proposición, entonces `not(P)` es una proposición que niega el valor de verdad asumido para P. Así, la negación de algo falso se toma por verdadera, y la negación de algo con valor cierto, se toma como falso.

Ejemplos
--------

Utilizamos [ésta nomenclatura](paradigma-l-c3-b3gico---un-poco-de-nomenclatura-consultas.html) para los ejemplos de consultas

### Consultas Individuales

Supongamos lo siguiente:

`esMalo(feinmann). `
`esMalo(hadad). `
`esMalo(echecopar).`

Y miremos los resultados de las siguientes consultas:

`?- esMalo(hadad).`
`true`
`?- not(esMalo(hadad)).`
`false`
`?- esMalo(bambi).`
`false`
`?- not(esMalo(bambi)).`
`true`

Ahora, miremos ésto:

`?- esMalo(34).`
`false`

¡Claro que no es malo! ¡El 34 no lastimaría una mosca! Porque éste paradigma se basa en el concepto de [Universo Cerrado](paradigma-l-c3-b3gico---introducci-c3-b3n-universo-cerrado.html). (Si no está en la base, es falso). Entonces, la siguiente consulta da verdadero:

`?- not(esMalo(34)).`
`true`

### Consultas Existenciales ó Variables

Recordemos cuál es el significado de la siguiente consulta:

`?- esMalo(_).`

Así, con la variable anónima, estoy preguntando si ***existe un malo***. Bueno, eso es cierto:

`?- esMalo(_).`
`true`

Ahora bien, veamos qué sucede si preguntamos si es cierto que "no existe un malo"

`?- not(esMalo(_)).`
`false`

Por último, recordemos que la variable anónima es igual que cualquier otra variable, por lo que éstas dos consultas tienen el mismo significado, de si "existe un malo":

`?- esMalo(_).`
`true`
`?- esMalo(X).`
`true.`

La única diferencia es que en el caso con la `X`, Prolog tratará de darme *ejemplos* de `X` que hacen verdadera esa consulta. Pero ambas quieren decir "existe algún malo". Entonces, la siguiente consulta dará falso:

`?- not(esMalo(X)).`
`false.`

Porque estoy preguntando si "no existe un malo".

Inversibilidad
--------------

Agreguemos lo siguiente al ejemplo:

`esPersona(feinmann). `
`esPersona(hadad). `
`esPersona(echecopar).`
`esPersona(bambi).`
`esPersona(fer).`
`esPersona(lucas).`

`esBueno(Persona):- not(esMalo(Persona)).`

### Problema

Veamos el resultado de la siguiente consulta:

`?- esBueno(lucas).`
`true.`
`?- esBueno(fer).`
`true.`

Y ahora tratemos de preguntar **si existe algún bueno** (claro que existen!):

`?- esBueno(X).`
`false.`

¿¿Qué pasó?? Además, nuestro predicado `esBueno` sigue teniendo problemas, observemos lo que sigue:

`?- esBueno(34).`
`true.`

Si bien el número 34 nos gusta mucho, sería conceptualmente incorrecto aceptar que fuera bueno, ya que nuestro dominio trabaja con personas, y es sobre ellas que, en éste caso, queremos verificar la propiedad de *ser buenas*.

### Interpretación

Mi regla dice que se cumple `esBueno(X)`, si se cumple `not(esMalo(Persona))`. Y por lo que vimos arriba, si hago una consulta *existencial* ó *variable* (es decir, que llegan las variables sin ligar con un individuo)... ¡Eso es falso! (Leer más arriba) Además, ya vimos que por el concepto de [Universo Cerrado](paradigma-l-c3-b3gico---introducci-c3-b3n-universo-cerrado.html) nuestra consulta `esBueno(34)` es cierta, ya que `not(esMalo(34))` es cierta.

### Solución

Entonces... ¿Qué hacemos?

*Generamos*.

[Generar](paradigma-logico---generacion.html) es agregar una condición que *sí* sea inversible antes del `not`, para que las variables lleguen ligadas al mismo. De ésta manera, transformamos la consulta dentro del `not` que podía llegar a ser una *consulta existencial* en una *consulta individual*, que funcionan como nosotros esperamos (Leer más ariba).

`esBueno(Persona) :- esPersona(Persona), not(esMalo(Persona)).`

Y así podemos consultar si "existe un bueno", y tener también ejemplos de buenos.

`?- esBueno(X).`
`X = bambi;`
`X = fer;`
`X = lucas.`

Para más conceptos sobre inversibilidad y generación, visitar el artículo [Paradigma\_L%C3%B3gico\_-\_inversibilidad](paradigma-l-c3-b3gico---inversibilidad.html)
