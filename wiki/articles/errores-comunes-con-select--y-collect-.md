Esto me lleva a recordar un error que aparece a menudo, no es exactamente el caso pero me viene bien para explicarlo. Fíjense una cosa: siempre que yo hago....

a := cualquier cosa

Eso es una asignación "destructiva"... sí ya sé, lo dije mil veces, pero lo que parece hasta acá fácil después igual genera confusiones.

¿Qué quiere decir eso de "destructiva"? Que se pierde lo que fuera que estaba referenciando la variable a, se pierde, o sea... no me importa lo que tenía la variable a antes de eso.

¿Por qué saco a colación esto ahora? Bueno el error que yo veo frecuentemente es este:

a := Set new. a := otroSet collect: \[... etc\]

¿Tiene sentido inicializar la variable a antes del collect:? Y no... si yo en la siguiente línea voy a hacer una asignación destructiva... justamente lo que sea que tiene la variable a se va a perder.

Visto de otra manera, acuérdense que dijimos el collect: me devuelve una colección NUEVA. Es decir, que no usa el valor de la variable a... porque me devuelve una colección nueva.

Y digo más, si entendés la asignación, podés ver que primero se evalúa el "lado derecho" (lo que está después del :=), y luego se asigna. Entonces "a" no interviene para nada en la evaluación de " otroSet collect: \[... etc\] " (salvo claro que aparezca a en la expresión). Digo esto para desmitificar la idea de que de alguna manera el collect: podría dejar sus valores en el Set que apunta la variable "a"... claramente eso no es posible.

(Esa confusión puede que venga de programar en c, donde a veces yo necesito "alocar memoria" para que otro procedimiento deje datos en esa memora... bueno, acá no es necesario alocar memoria, eso pasa solo! wiii! jaja. Además que aún en estructurado para que el proc pueda usar lo que yo "aloqué" debería pasárselo por parámetro y no como valor de retorno, esa característica sí es compartida.)

Obviamente, el ejemplo del collect: vale para select: y todo método que devuelva una colección. ¿No?
