---
layout: article
title: Conocimiento de dominio y refactoring
---

¿Se puede iniciar un refactoring sin conocimiento del dominio?

¿Se puede refa La respuesta corta es: no, no se puede (\*) realizar un refactor sin conocimiento del dominio.

Ahora sí, la respuesta larga.

Primero, aclaremos algunas ideas:

"dicen que no necesitan conocerlo para hacer un refactoring"

Ojo, lo que comentamos en clase fue que no necesito conocer el dominio para identificar potenciales fallas en el diseño, que se manifiestan a través del código. Es decir, sin conocimiento de dominio podemos sólo identificar code smells, que tienen asociados posibles refactors.

Sí, remarco el posible/potencial porque es importante que entendamos que se trata sólo de una heurística. Quizás el diseño sí es correcto, o quizás el refactor que deberíamos encarar no tiene nada que ver con lo que propone el code smell. Por eso, también ojo con esto:

"pero no hace falta saber nada acerca del dominio para darte cuenta que tenes q delegar en subclases en ves de preguntar por el tipo"

Esto tampoco es correcto, si bien es muy probable que haya que emplear polimorfismo (y no necesariamente utilizar herencia), necesitaremos conocer al dominio y a la tecnología como para poder justificar ese cambio (\*\*) . De lo contrario, si pensaramos que para toda estructura de código que se corresponde con un code smell hay un cambio que tenemos que realizar, estariamos (casi) insinuando que hay aspectos del diseño automatizables. Yo prefiero descreer de esto.

En resumen hasta acá, parafraseándote, no necesito conocer al dominio para encontrar code smells, pero sí lo necesito para saber si efectivamente hay una falla de diseño, y en tal caso, determinar el refactor más apropiado que los subsane.

Pero este otro comentario:

"si el sistema tiene un elevado nivel de acoplamiento, hacer un refactoring de algo te lleva a cambiar 90 cosas más...que a menos que sepas como viene el sistema y cuál es la funcionalidad de cada parte y su dependencia, no lo vas a poder hacer"

me lleva a otra pregunta: supongamos que tenemos conocimiento del dominio; ¿podemos realizar un refactor sin conocimiento del diseño actual? Se los dejo para pensar, sería interesante si alguien se anima proponer una solución al dilema (sic).

Y finalmente, buena observación:

"después de cada modificación en el diseño hay q actualizar la documentación"

sí, si modificamos el diseño de un sistema, deberíamos impactarlo en la documentación que llevemos de este. Como eso puedo ser tedioso, y en los momentos iniciales del desarrollo de un sistema los cambios de diseño son constantes, es frecuente postergar la realización de una documentación exhaustiva hasta que el diseño a más alto nivel (llamémoslo arquitectura) quede estabilizado.

Saludos!

(\*) "no se puede" en sentido informal: es ciertamente posible hacerlo, pero no es una gran idea, por lo expuesto precisamente por vos, al menos para bases de código (proyectos) no triviales.

Por ejemplo, en nuestro ejercicio de monedero, el dominio era muy simple: poner y sacar dinero en una cuenta, llevando un historial del mismo, y validando algunas reglas de negocio, por eso con apenas algunos minutos de análisis podíamos saber qué refactorizar. Pero en sistemas con dominios más complejos y muchas más tecnologías involucradas, hubiéramos necesitado mucho más tiempo para determinar exactamente qué hacer.
