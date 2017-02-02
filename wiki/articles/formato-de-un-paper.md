---
layout: article
title: Formato de un paper
---

No todas las secciones son necesarias, pueden haber otras secciones o con otros nombres. Este solo es un template de guía.

Título
------

Hay que elegir un buen título :)

Abstract
--------

El abstract no es otra cosa que un resumen de todo el paper. Debería contener aproximadamente una oración para cada uno de los siguientes temas:

-   contexto
-   problema
-   porque es un problema
-   solucion que se presenta
-   como lo que presentamos soluciona el problema
-   quizas algun ejemplo

Después de leer el abstract, el lector debería saber de qué se trata el paper a groso modo, y poder decidir si quiere seguir leyéndolo o no, si le sirve o no...

Introduction
------------

La introducción le muestra al lector que hicimos la tarea. Sí, que sabemos de qué estamos hablando, conocemos los problemas, conocemos lo que otra gente hizo. Aca se explica mucho del contexto en el que nos paramos y brevemente los otros puntos, ya que van a ser mejor explicados en las secciones que siguen.

-   Context: contar el contexto del problema, qué tecnología, qué ambiente. ¿es relacionado a web apps? ¿A lenguajes con chequeo estático de tipos? ¿Lenguajes con herencia múltiple?
-   Problems: contar brevemente los problemas que se quieren solucionar
-   Current Solutions: contar brevemente las soluciones existentes que existen que atacan el problema
-   Contributions: que agregamos nosotros a la investigación en el tema
-   Outline: contar de qué se tratan las secciones que siguen. Es el típico "In section 2 we present... In section 3 we show our implementation of bleh"

Problems
--------

-   Constraints: ¿Sobre qué limitaciones trabajamos? ¿Ambientes con mucha seguridad? ¿Sin conexión de red? ¿Vms restrictivas?

Luego enumeramos los problemas actuales que vamos a encarar:

-   Problem1
-   Problem2
-   Your solution in a nutshell: contamos nuestra solución muy brevemente, contando en muy pocas lineas qué problemas resolvemos. Estamos invitando a que nos sigan a la próxima sección.

My Solution
-----------

Contamos la solución. Hice X, Y y Z, que solucionan los problemas A, B y C, así y asá. No comparamos con otra gente ni otras soluciones. Esta sección es toda nuestra.

Discussion
----------

La discusión es donde:

-   nuestra solución se compara con las existentes
-   abrimos la puerta a líneas que no se llegaron a explorar aca, que podrían encararse en el futuro.

Implementation
--------------

Aca podemos hablar de nuestra implementación en particular. Es la parte menos de investigación(?).

Evaluation
----------

Qué tan buena es nuestra implementación y como soluciona los problemas que enumeramos. Aca podemos poner benchmarks, métricas, etc.

Related work
------------

Aca normalmente se hablar de gente trabajando en cosas parecidas, no necesariamente sobre el mismo campo, o buscando la misma solución. Es bastante similar a Discussion, y por eso ambas secciones pueden ser una sola.

Conclusion
----------

Es un resumen post lectura, o sea, el lector ya tiene nuestro laburo en la cabeza: "Nuestra solución X pareciera ser una buena solución al problema Y, aunque no tanto para el problema Z. Queda por explorar el camino J, que encaró inicialmente el autor Fulanito\[cita\]"
