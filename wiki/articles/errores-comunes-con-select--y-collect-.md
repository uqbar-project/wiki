---
layout: article
title: Errores comunes con select  y collect 
---

Orden incorrecto entre collect: y select:
-----------------------------------------

Como se explica en [Mensajes de colecciones](mensajes-de-colecciones.html), el select: es para filtrar una colección en base a un criterio mientras que el collect: es para recolectar los resultados de enviar un mensaje a cada objeto de la colección original.

Supongamos que queremos obtener una colección con los promedios de los alumnos que aprobaron, podríamos plantearlo como

`(alumnos select: [:unAlumno | unAlumno aprobo ]) collect: [:unAlumno | unAlumno promedio ]`

Si en vez de eso hicieramos

`(alumnos collect: [:unAlumno | unAlumno promedio ]) select: [:unAlumno | unAlumno aprobo ]`

Esto no va a funcionar, porque el collect: va a retornar una lista de números, no de alumnos, con lo cual el filtrado no se va a poder hacer.

Si lo que queremos es en cambio obtener los promedios &gt; 4 de los alumnos, ahí no habría problema en hacer primero el collect: y luego el select:, porque el filtrado se haría sobre promedios y no sobre alumnos:

`(alumnos collect: [:unAlumno | unAlumno promedio ]) select: [:unPromedio | unPromedio > 4 ]`

Inicialización inecesaria de variables
--------------------------------------

Fíjense una cosa: siempre que yo hago....

` a := cualquier cosa`

Eso es una asignación "destructiva"... sí ya sé, lo dije mil veces, pero lo que parece hasta acá fácil después igual genera confusiones.

¿Qué quiere decir eso de "destructiva"? Que se pierde lo que fuera que estaba referenciando la variable a, se pierde, o sea... no me importa lo que tenía la variable a antes de eso.

¿Por qué saco a colación esto ahora? Bueno el error que yo veo frecuentemente es este:

` a := Set new.`
` a := otroSet collect: [... etc]`

¿Tiene sentido inicializar la variable a antes del collect:? Y no... si yo en la siguiente línea voy a hacer una asignación destructiva... justamente lo que sea que tiene la variable a se va a perder.

Visto de otra manera, acuérdense que dijimos el collect: me devuelve una colección NUEVA. Es decir, que no usa el valor de la variable a... porque me devuelve una colección nueva.

Y digo más, si entendés la asignación, podés ver que primero se evalúa el "lado derecho" (lo que está después del :=), y luego se asigna. Entonces "a" no interviene para nada en la evaluación de " otroSet collect: \[... etc\] " (salvo claro que aparezca a en la expresión). Digo esto para desmitificar la idea de que de alguna manera el collect: podría dejar sus valores en el Set que apunta la variable "a"... claramente eso no es posible.

(Esa confusión puede que venga de programar en c, donde a veces yo necesito "alocar memoria" para que otro procedimiento deje datos en esa memora... bueno, acá no es necesario alocar memoria, eso pasa solo! wiii! jaja. Además que aún en estructurado para que el proc pueda usar lo que yo "aloqué" debería pasárselo por parámetro y no como valor de retorno, esa característica sí es compartida.)

Obviamente, el ejemplo del collect: vale para select: y todo método que devuelva una colección. ¿No?
