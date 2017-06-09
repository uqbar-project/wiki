---
layout: article
title: Paradigma logico   negacion
---

El predicado de [orden superior](orden-superior.html) más simple, y a la vez muy utilizado, es el `not/1`, que representa una negación. Si `P` es una proposición, entonces `not(P)` es una proposición que niega el valor de verdad asumido para P. Así, la negación de algo falso se toma por verdadera, y la negación de algo con valor cierto, se toma como falso.

Ejemplos
--------

### Consultas Individuales

Supongamos lo siguiente:

```
esMalo(feinmann). 
esMalo(hadad). 
esMalo(echecopar).
```

Y miremos los resultados de las siguientes consultas:

```
?- esMalo(hadad).
true
?- not(esMalo(hadad)).
false
?- esMalo(bambi).
false
?- not(esMalo(bambi)).
true
```

Ahora, miremos ésto:

```
?- esMalo(34).
false
```

¡Claro que no es malo! ¡El 34 no lastimaría una mosca! Porque éste paradigma se basa en el concepto de [Universo Cerrado](paradigma-logico---introduccion.html). (Si no está en la base, es falso). Entonces, la siguiente consulta da verdadero:

```
?- not(esMalo(34)).
true
```

### Consultas Existenciales ó Variables

Recordemos cuál es el significado de la siguiente consulta:

`?- esMalo(_).`

Así, con la variable anónima, estoy preguntando si ***existe un malo***. Bueno, eso es cierto:

```
?- esMalo(_).
true
```

Ahora bien, veamos qué sucede si preguntamos si es cierto que "no existe un malo"

```
?- not(esMalo(_)).
false
```

Por último, ¿se podrá usar una variable que no sea la variable anónima dentro de una negación?

Para responder a eso vamos a tener que hablar sobre inversibilidad.

Inversibilidad
--------------

Agreguemos lo siguiente al ejemplo:

```
esPersona(feinmann). 
esPersona(hadad). 
esPersona(echecopar).
esPersona(bambi).
esPersona(fer).
esPersona(lucas).

esBueno(Persona):- 
  not(esMalo(Persona)).
```

### Problema

Veamos el resultado de la siguiente consulta:

```
?- esBueno(lucas).
true.
?- esBueno(fer).
true.
```

Y ahora tratemos de preguntar **si existe algún bueno** (claro que existen!):

```
?- esBueno(X).
false.
```

¿¿Qué pasó?? ¿No debería ser capaz de decirnos quiénes son buenos?

### Interpretación

Mi regla dice que se cumple `esBueno(X)`, si se cumple `not(esMalo(Persona))`.

Sabiendo que la variable anónima es igual que cualquier otra variable, sólo que la usamos cuando no nos interesa conocer qué individuo hace que se cumpla la consulta, éstas dos consultas tienen el mismo valor de verdad, que para nuestra base de conocimientos sería verdadero ya que existe alguien que es malo:

```
?- esMalo(_).
?- esMalo(X).
```

Entonces, la siguiente consulta (equivalente a `esBueno(X)`) dará falso:

```
?- not(esMalo(X)).
false.
```

Porque estoy preguntando si es cierto que "no existe alguien malo".

Normalmente si usamos una variable no anónima, Prolog tratará de darnos *ejemplos* que hacen verdadera esa consulta. Pero en el caso de la negación, es imposible para el motor encontrar ejemplos de individuos que satisfagan la consulta, porque son infinitos! Por ejemplo, el 34 no es malo...

```
?- esBueno(34).
false.
```

### Solución

Entonces... ¿Qué hacemos?

*Generamos*.

[Generar](paradigma-logico---generacion.html) es agregar una condición que *sí* sea inversible antes del `not`, para que las variables lleguen ligadas al mismo. De ésta manera, transformamos la consulta dentro del `not` que podía llegar a ser una *consulta existencial* en una *consulta individual*, que funcionan como nosotros esperamos (Leer más ariba).

```
esBueno(Persona) :-
  esPersona(Persona), 
  not(esMalo(Persona)).
```

Y así podemos consultar si "existe un bueno", y tener también ejemplos de buenos.

```
?- esBueno(X).
X = bambi;
X = fer;
X = lucas.
```

Para más información sobre inversibilidad y generación, visitar el artículo [Paradigma Lógico - inversibilidad](paradigma-logico---inversibilidad.html)

Además, nuestro nuevo predicado `esBueno` se va a comportar distinto que antes ante esta consulta individual:

```
?- esBueno(34).
false.
```

Si bien el número 34 nos gusta mucho, sería conceptualmente incorrecto aceptar que fuera bueno, ya que nuestro dominio trabaja con personas, y es sobre ellas que, en éste caso, queremos verificar la propiedad de *ser buenas*. Con el agregado que hicimos no sólo resolvimos el problema de las consultas existenciales, sino que además sirvió para acotar el dominio de aquellos que pueden ser buenos.

