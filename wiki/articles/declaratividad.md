---
layout: article
title: Declaratividad
---

La declaratividad es una característica de algunas herramientas que permiten o fuerzan la separación entre el conocimiento del dominio de un programa y la manipulación de dicho conocimiento. Dichas herramientas pueden ser de diversa naturaleza, tanto prácticas como lenguajes, entornos, frameworks, etc o conceptuales como paradigmas o arquitecturas.

El concepto de declaratividad se contrapone a la tradicional programación imperativa en la cual el conocimiento y la lógica se encuentran muchas veces mezclados dentro de la misma porción de código resultando difícil determinar donde comienza uno y dónde termina el otro.

Las herramientas declarativas permiten muchas veces un más alto grado de reutilización y de abstracción en tareas repetitivas y por eso la declaratividad es hoy en día una característica fundamental en muchos entornos de programación para sistemas de mediana o gran envergadura, ya que es una herrramienta importante para organizar y simplificar la construcción de un sistema complejo.

Conocimiento y manipulación
---------------------------

En un programa construido de forma declarativa se produce una separación entre la descripción del problema por un lado y los algoritmos o estrategias para encontrar la solución por el otro.

Un error común es hablar de la separación entre el *qué* y el *cómo*. Si bien esta idea puede servir como una primera aproximación al concepto, en realidad cualquier función - predicado - método - procedimiento puede verse como una separación entre el *qué* (dado por el nombre de la función, lo único que necesita saber el invocador) y el *cómo* (la implementación de la función).

En un programa imperativo suelen estar mezcladas la descripción del problema con la estrategia de resolución, a tal punto que muchas veces es difícil de detectar cuál es el problema que se está tratando de solucionar.

Para verlo mejor es conveniente bajar a un ejemplo, para eso vamos a pensar en el sistema de correlatividades de la facultad. Pensando imperativamente, el algoritmo de solución sería algo así:

1.  A partir de un alumno obtener la carrera que está cursando y con eso las materias de esa carrera, almacenarlas en una variable auxiliar.
2.  Eliminar de esa colección de materias aquellas que el alumno ya haya cursado.
3.  Recorrer la lista de las materias restantes y para cada una:
    1.  Obtener su lista de corelativas
    2.  Recorrerla y para cada correlativa verificar si el alumno cursó esa materia
    3.  En caso de no haberla cursado, eliminar la materia de la colección auxiliar

4.  Las materias que quedaron en la colección son las que se pueden cursar.

En cambio, la definición declarativa eliminará todos los conceptos programáticos como variables auxiliares, recorrer colecciones o ir eliminando elementos de la colección. Más aún, la versión declarativa no tendrá un concepto de orden, simplemente intentará describir el problema de la forma más abstracta posible, solamente tratando de contestar la pregunta, ¿qué materias puede cursar un alumno?

1.  Un alumno puede cursar las materias de su carrera que no haya cursado aún y cuyas correlativas sí haya cursado.

En este punto seguramente se preguntarán si es posible hacer un programa que exprese solamente eso, sin toda la lógica adicional necesaria, bueno aquí está el código prolog que dice exactamente eso:

```prolog
    puedeCursar(Alumno, Materia):-
      carrera(Alumno, Carrera), materia(Carrera, Materia),       % Es una materia de la carrera del alumno
      not(curso(Alumno, Materia)),                               % No cursó la materia
      forall(correlativa(Materia, Corr), curso(Alumno, Corr)).   % Cursó todas las correlativas
```

Elementos en un programa declarativo
------------------------------------

Un programa declarativo separa claramente los siguientes elementos:

-   El objetivo
-   El conocimiento
-   El motor que manipula el conocimiento para lograr el objetivo deseado

En el ejemplo anterior, el objetivo es la consulta realizada sobre qué materias puede cursar un alumno. El conocimiento es la información que se encuentra en la base de conocimiento sobre las materias disponibles en la carrera y cuáles ya cursó el alumno en cuestión. El motor de Prolog toma el conocimiento y resuelve la consulta realizada en base al programa y deduce todas las posibles relaciones que la satisfagan.

El mecanismo utilizado por el motor de Prolog llamado [Backtracking](backtracking.html) prueba todas las posibilidades para solucionar el problema, no hace falta programar este algoritmo para cada problema particular, con lo cual podemos concentrarnos exclusivamente en el objetivo de nuestro programa y no en la complejidad algorítmica general que permite procesar la información.

Declaratividad en los distintos paradigmas
------------------------------------------

Dentro de los paradigmas vistos, el paradigma funcional y el lógico tienen una naturaleza declarativa. Eso no quiere decir que no se pueda ser declarativo programando bajo el marco de otro paradigma, de la misma forma en que se puede programar de forma poco declarativa en lógico y funcional (por ejemplo, la recursividad no es una herramienta muy declarativa que digamos).

A medida que logramos abstraer nuestros problemas podemos armar nuestros propios motores que nos permitan trabajar de forma más declarativa.

Herramientas declarativas en el *mundo real*
--------------------------------------------

El lenguaje SQL para trabajar con bases de datos relacionales es un claro ejemplo de declaratividad que se usa ampliamente en la indusria.

Los motores de bases de datos, a partir de consultas provistas por el usuario que especifican el origen de los datos (FROM), los filtros a aplicar sobre los posibles resultados (WHERE), las transformaciones a realizar sobre las filas resultantes (SELECT) y criterios de ordenamiento (ORDER BY) por ejemplo, realizan búsquedas complejas relacionando las tablas de origen solicitadas de la forma más optimizada posible en base a los índices que el usuario defina y estadísticas que el mismo motor realiza.

El algoritmo utilizado por el motor está muy separado del conocimiento (qué entidades existen, cómo se relacionan entre ellas), lo cual permite al usuario abstraerse de esta lógica de búsqueda y concentrarse exclusivamente en el modelado de datos.
