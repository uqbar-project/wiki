---
layout: article
title: Encapsulamiento
---

¿Qué es el encapsulamiento?
---------------------------

Quien usa un objeto sólo ve lo que necesita para poder interactuar con él, que es el comportamiento que exhibe. Los detalles internos quedan encapsulados en el objeto. Así quedan acotadas y explicitadas las formas posibles de interactuar con un objeto.

El ocultamiento de detalles de implementación como ser los atributos de un objeto permite separar mejor las responsabilidades y evitar efectos inesperados como resultado de la modificación del valor de las variables por entidades externas.

El uso de setters y getters (mensajes para modificar y conocer el valor de un atributo respectivamente), también conocidos como accessors, es importante para que el objeto que tiene esos atributos pueda controlar el uso de los mismos y para que los que usan al objeto que los tiene no sufran un impacto muy grande si la implementación del mismo cambia.

Ejemplo
-------

Supongamos que representamos a los lugares a los que puede volar nuestra amiga Pepita la golondrina como objetos que conocen su kilometraje en una ruta y nos saben decir a qué distancia se encuentran de otro lugar. Si su estado interno se modificara de modo que su ubicación se represente por una coordenada (x e y), sólo los lugares deberían verse afectados por este cambio ya que a Pepita sólo le interesa conocer la distancia entre dos lugares.

A medida que el sistema crece esta característica toma más importancia ya que no es fácil determinar todos los lugares en los cuales algo se está usando y qué impacto tiene ese uso.

Es importante entender que acceder a los atributos de un objeto mediante mensajes no es suficiente para afirmar que no se rompe el encapsulamiento del objeto. Supongamos que queremos calcular la distancia que tiene que volar pepita para llegar a otro lugar, es muy común ver cosas como:

**`Smalltalk:`**
`pepita lugarActual kilometraje - otroLugar kilometraje`

**`Wollok:`**
`pepita.lugarActual().kilometraje() - otroLugar().kilometraje()`

(donde lugarActual y kilometraje son los getters de los respectivos objetos receptores) para trabajar con el número resultante en vez de delegar en el objeto que puede resolver el problema de la distancia a otro lugar. Si bien sólo se están usando getters, al preguntarle el kilometraje al lugar cuando lo que nos interesaba era la distancia nos estamos acoplando a cómo representa su ubicación, y por ende si se quisiera cambiar a coordenadas, los usuarios del mensaje kilometraje se van a ver afectados.
