Holas,

Tal vez no es la mejor fecha para mandar un mail así pero bueno ...

Últimamente leí muchas preguntas sobre estos conceptos y la idea de esto es saber su interpretación (más que nada de los primeros 2) y las relaciones entre los tres.

La motivación no es llegar a una definición formal de cada uno sino saber qué entendemos por cada concepto y qué es lo que nos interesa.

En los finales, muchas veces, hay algunas diferencias y si llegamos a una "definición" en común (o al menos logramos la desaparición de algunas ambigüedades) supongo que estaría bueno.

Operación = aplicar una función, evaluar un predicado, enviar un mensaje, etc.

1) Transparencia Referencial Definición 1: Hay transparencia referencial cuando al realizar una operación con los mismos valores siempre da el mismo resultado Definición 2: Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto

2) Efecto de Lado Definición: Hay efecto de lado cuando un cambio de estado sobrevive a la realización de una operación

3) Asignación Destructiva Definición: Asignar destructivamente es reemplazar el valor de una variable por otro valor Nota1: La unificación no se considera asignación (al momento de ligar no había ningún valor anterior, ¿sería más bien una inicialización?) Nota2: Unificar es encontrar una sustitución capaz de igualar 2 términos Nota3: Cuando se efectiviza está sustitución hablamos de ligado de variables (tal valor se ligó a tal variable)

Cuando hablamos de que "algo" tiene transparencia referencial ... ¿Ese "algo" puede ser un paradigma, un lenguaje, la realización de una operación o una pieza de código?

Cuando hablamos de que "algo" tiene efecto de lado ... ¿Ese "algo" siempre se refiere a una operación o una pieza de código? Acá no se puede hablar de paradigma con efecto de lado o lenguaje, ¿no?

Cuando hablamos de que "algo" tiene asignaciones destructivas ... ¿Ese "algo" puede ser un paradigma, un lenguaje, la realización de una operación o una pieza de código?

- Que NO haya asignaciones destructivas me asegura que NO haya efecto de lado (Igual a, solo hay efecto de lado en presencia de asignaciones destructivas) - Que haya asignaciones destructivas NO me asegura que haya efecto de lado - Que no haya efecto de lado NO me asegura que haya transparencia referencial (Depende del punto de vista, más que nada ver el Ejemplo 1) - Solo puede haber transparencia referencial cuando hay ausencia de efecto de lado (Esto solo con la definición 2 de transparencia referencial, porque sino el ejemplo 2 lo contradice)

Los ejemplos son en smalltalk porque en los lenguajes que implementan paradigmas declarativos al """"no tener asignaciones destructivas"""" no interesa el concepto de efecto de lado.

Ejemplo 1) Transparencia Referencial: NO (Con las 2 definiciones de transparencia referencial) Efecto de Lado: NO Asignación Destructiva: NO

Date today

Evaluarlo con los mismos parámetros (o sea ninguno) en días distintos va a dar resultados distintos. Reemplazar la operación por el resultado una vez que cambia el día se rompe todo. Asignación destructiva y efecto de lado hay, pero en la CPU que actualiza la variable que indica el tiempo, no en el mensaje today que consulta ese valor (no se si es tan así, pero es a modo ilustrativo).

El efecto de lado de otra operación afecta a esta operación y le hace perder la transparencia referencial, a pesar de que esta operacion por si misma NO tiene efecto de lado.

¿Ejemplos como este hacen que transparencia referencial y efecto de lado no sean conceptos opuestos?

Ejemplo 2) Transparencia Referencial: SI con la definición 1, pero NO con la definición 2 Efecto de Lado: SI (Por ende hay asignaciones destructivas)

1.  LaColeccionConEfectoDeLado

add: unElemento

`    "El add: siempre devuelve lo que se agrega acá se redefine para avisarle al elemento que fue agregado (no se me ocurrió nada mejor)"`
`     unElemento teAgregaronEn: self.`
`     ^super add: unElemento.`

Asumiendo que los parámetros siempre entienden el mensaje \#teAgregaronEn: no importa cuantas veces se realicen estás operaciones siempre devuelven el parámetro

LaColeccionConEfectoDeLado new add: 4. "Devuelve 4" LaColeccionConEfectoDeLado new add: pepita. "Devuelve pepita"

Pero obviamente no es lo mismo escribir

(LaColeccionConEfectoDeLado new add: 4) que esto (4)

Mi propuesta es que tomemos como definición (o como idea) la definición 2 de transparencia referencial.

Ejemplo 3) "Supongo que con este ejemplo no habrá quejas :P" Transparencia Referencial: SI (con las 2 definiciones) Efecto de Lado: NO Asignaciones Destructivas: SI

1.  Number

factorial

`    | resultado |`
`    resultado := 1.`
`    self < 0 ifTrue: [ self error: 'Como que no va pedirle el factorial a un número negativo' ].`
`    1 to: self do: [ :indice | resultado := resultado * indice ].`
`    ^resultado`

Bueno más que nada es eso, espero sus opiniones al respecto

Saludos

PD: De yapa les mando un libro (acá) que me pareció muy interesante ya que toca muchos de los temas que damos nosotros (y muchos más) con un enfoque distinto. Básicamente, en vez de presentar varios lenguajes que implementan distintos paradigmas, usan un solo lenguaje que implementa varios (me gusta más nuestro enfoque, pero es interesante de todas formas). --
