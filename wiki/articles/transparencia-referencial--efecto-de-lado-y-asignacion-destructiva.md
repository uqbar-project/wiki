Holas,

Tal vez no es la mejor fecha para mandar un mail así pero bueno ...

Últimamente leí muchas preguntas sobre estos conceptos y la idea de esto es saber su interpretación (más que nada de los primeros 2) y las relaciones entre los tres.

La motivación no es llegar a una definición formal de cada uno sino saber qué entendemos por cada concepto y qué es lo que nos interesa.

En los finales, muchas veces, hay algunas diferencias y si llegamos a una "definición" en común (o al menos logramos la desaparición de algunas ambigüedades) supongo que estaría bueno.

------------------------------------------------------------------------

Definiciones
------------

**Operación = aplicar una función, evaluar un predicado, enviar un mensaje, etc.**

**1) Transparencia Referencial** *Definición 1:* Hay transparencia referencial cuando al realizar una operación con los mismos valores siempre da el mismo resultado *Definición 2:* Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto

**2) Efecto de Lado** *Definición:* Hay efecto de lado cuando un cambio de estado sobrevive a la realización de una operación. Por ejemplo, una operación puede modificar una variable global, modificar uno de sus argumentos, escribir datos a la pantalla o a un archivo, o hacer uso de otras operaciones que tienen efecto de lado.

**3) Asignación Destructiva** *Definición:* Asignar destructivamente es reemplazar el valor de una variable por otro valor *Nota1:* La unificación no se considera asignación (al momento de ligar no había ningún valor anterior, ¿sería más bien una inicialización?) *Nota2:* Unificar es encontrar una sustitución capaz de igualar 2 términos *Nota3:* Cuando se efectiviza está sustitución hablamos de ligado de variables (tal valor se ligó a tal variable)

Ejemplos
--------

Cuando hablamos de que "algo" tiene transparencia referencial, efecto de lado o asignación destructiva, ese "algo" puede ser la realización de una operación, de un lenguaje en particular o de un paradigma (es natural hablar de existencia de efecto de lado en objetos y estructurado y no en lógico y funcional, al igual que de asignación destructiva). Sin embargo, este texto está orientado a las operaciones concretas.

Estos tres conceptos suelen ir de la mano y si bien pueden darse relaciones entre ellas está bueno saber detectar la aparición de estos conceptos individualmente. Una relación que surge de la segunda definición de transparencia referencial es que para ésta se de, no puede haber efecto de lado, ya que si el sistema se modifica, no es lo mismo ejecutar esa operación que reemplazar por el resultado.

A continuación mostramos algunos ejemplos en smalltalk, ya que permite la aparición de todas estas características, para dejar más en claro de qué manera podemos identificarlas.

**Ejemplo 1)**

*Date today*

Transparencia Referencial: NO (Con las 2 definiciones de transparencia referencial)

Efecto de Lado: NO

Asignación Destructiva: NO

Evaluarlo con los mismos parámetros (o sea ninguno) en días distintos va a dar resultados distintos. Reemplazar la operación por el resultado una vez que cambia el día se rompe todo. Asignación destructiva y efecto de lado hay, pero en la CPU que actualiza la variable que indica el tiempo, no en el mensaje today que consulta ese valor (no se si es tan así, pero es a modo ilustrativo).

El efecto de lado de otra operación afecta a esta operación y le hace perder la transparencia referencial, a pesar de que esta operacion por si misma NO tiene efecto de lado.

¿Ejemplos como este hacen que transparencia referencial y efecto de lado no sean conceptos opuestos?

**Ejemplo 2)**

`#LaColeccionConEfectoDeLado`
` >>add: unElemento`
`    "El add: siempre devuelve lo que se agrega. Acá se redefine para avisarle al elemento que fue agregado (no se me ocurrió nada mejor)"`
`     unElemento teAgregaronEn: self.`
`     ^super add: unElemento.`

Transparencia Referencial: SI con la definición 1, pero NO con la definición 2. En un final, si se da un caso como este y están en duda, justifiquen por qué sí o no.

Efecto de Lado: SI, porque la colección, luego de recibir el mensaje add: se modifica.

Asignación destructiva: no se la ve directamente en éste método, si bien puede estar presente en teAgregaronEn: o en add: No sé si tiene mucho sentido hablar de asignación destructiva en este ejemplo.

Asumiendo que los parámetros siempre entienden el mensaje \#teAgregaronEn: no importa cuantas veces se realicen estás operaciones siempre devuelven el parámetro

LaColeccionConEfectoDeLado new add: 4. "Devuelve 4" LaColeccionConEfectoDeLado new add: pepita. "Devuelve pepita"

Pero obviamente no es lo mismo escribir

(LaColeccionConEfectoDeLado new add: 4) que esto (4)

Mi propuesta es que tomemos como definición (o como idea) la definición 2 de transparencia referencial.

**Ejemplo 3)**

`#Number`
` >>factorial`
`    | resultado |`
`    resultado := 1.`
`    self < 0 ifTrue: [ self error: 'Como que no va pedirle el factorial a un número negativo' ].`
`    1 to: self do: [ :indice | resultado := resultado * indice ].`
`    ^resultado`

Transparencia Referencial: SI (con las 2 definiciones)

Efecto de Lado: NO

Asignaciones Destructivas: SI

Supongo que con este ejemplo no hace falta explicar el por qué.

Bueno más que nada es eso, espero sus opiniones al respecto

Saludos

*Otros Ejemplos*

**El efecto de lado, dependiendo del contexto**

1.  Collection

select: aBlock

`   | newCollection |`
`   newCollection := self species new.`
`   self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].`
`   ^newCollection`

select: no tiene efecto de lado en sí mismo, porque no modifica la colección original (self), sino que crea una nueva de su mismo tipo. Sin embargo podemos ver dentro de select: efectos colaterales en la asignación de newCollection y en el add:. Ambos pueden ser considerados como efectos colaterales dentro de la ejecución del método, pero quien usa select: no se da cuenta de eso y para él no tiene efecto de lado. Por otra parte, aunque el select: garantiza que no genera efectos de lado, no nos garantiza que el bloque que viene como parámetro no pueda tenerlo, por lo que uno debe tener cuidado con eso.
