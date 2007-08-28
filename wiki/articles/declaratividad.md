La declaratividad es una característica de algunas herramientas que permiten o fuerzan la separación entre el conocimiento del dominio de un programa y la manipulación de dicho conocimiento. Dichas herramientas pueden ser de diversa naturaleza, tanto prácticas como lenguajes, entornos, frameworks, etc o conceptuales como paradigmas o arquitecturas.

El concepto de declaratividad se contrapone a la tradicional [programación imperativa](programacion-imperativa.html) en la cual el conocimiento y la lógica se encuentran muchas veces mezclados dentro de la misma porción de código resultando difícil determinar donde comienza uno y dónde termina el otro.

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
    3.  En caso de no haberla cursado, elimar la materia de la colección auxiliar

4.  Las materias que quedaron en la colección son las que se pueden cursar.

En cambio, la definición declarativa eliminará todos los conceptos programáticos como variables auxiliares, recorrer colecciones o ir eliminando elementos de la colección. Más aún, la versión declarativa no tendrá un concepto de orden, simplemente intentará describir el problema de la forma más abstracta posible, solamente tratando de contestar la pregunta, ¿qué materias puede cursar un alumno?

1.  Un alumno puede cursar las materias de su carrera que no haya cursado aún y cuyas correlativas sí haya cursado.

En este punto seguramente se preguntarán si es posible hacer un programa que exprese solamente eso, sin toda la lógica adicional necesaria, bueno aquí está el código prolog que dice exactamente eso: <code>

    puedeCursar(Alumno, Materia):-
      carrera(Alumno, Carrera), materia(Carrera, Materia),       // Es una materia de la carrera del alumno
      not(curso(Alumno, Materia)),                               // No cursó la materia
      forall(correlativa(Materia, Corr), curso(Alumno, Corr)).   // Cursó todas las correlativas
    </code>

Elementos en un programa declarativo
------------------------------------

Declaratividad en los distintos paradigmas
------------------------------------------

Herramientas declarativas en el *mundo real*
--------------------------------------------
