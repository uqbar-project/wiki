Holas,

Tal vez no es la mejor fecha para mandar un mail así pero bueno ...

Últimamente leí muchas preguntas sobre estos conceptos y la idea de esto es saber su interpretación (más que nada de los primeros 2) y las relaciones entre los tres.

La motivación no es llegar a una definición formal de cada uno sino saber qué entendemos por cada concepto y qué es lo que nos interesa.

En los finales, muchas veces, hay algunas diferencias y si llegamos a una "definición" en común (o al menos logramos la desaparición de algunas ambigüedades) supongo que estaría bueno.

------------------------------------------------------------------------

Definiciones
------------

Operación:aplicar una función, evaluar un predicado, enviar un mensaje, etc.
Transparencia Referencial  
Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto.

Una definición alternativa dice: Hay transparencia referencial cuando al realizar una operación con los mismos valores siempre da el mismo resultado. Si bien esta parece más fácil de entender, no es tan precisa como la primera; puede ser útil para dar los primeros pasos, pero para el final hay que terminar de entender la otra.

<!-- -->

Efecto de Lado  
Hay efecto de lado cuando un cambio de estado sobrevive a la realización de una operación. Por ejemplo, una operación puede modificar una variable global, modificar uno de sus argumentos, escribir datos a la pantalla o a un archivo, o hacer uso de otras operaciones que tienen efecto de lado.

<!-- -->

Asignación Destructiva  
Asignar destructivamente es reemplazar el valor de una variable por otro valor.

**Nota1:** La unificación no se considera asignación (al momento de ligar no había ningún valor anterior, ¿sería más bien una inicialización?)

**Nota2:** Unificar es encontrar una sustitución capaz de igualar 2 términos

**Nota3:** Cuando se efectiviza está sustitución hablamos de ligado de variables (tal valor se ligó a tal variable)

Ejemplos
--------

Cuando hablamos de que "algo" tiene transparencia referencial, efecto de lado o asignación destructiva, ese "algo" es la realización de una operación, de un lenguaje en particular o de un paradigma.

Estos tres conceptos suelen ir de la mano y si bien pueden darse relaciones entre ellas es saludable poder detectar la aparición de cada uno de ellos individualmente. Una relación que surge de la definición de transparencia referencial es que para ésta se dé, no puede haber efecto colateral, ya que si el estado del sistema se modifica, no es lo mismo ejecutar esa operación que reemplazar por el resultado.

A continuación mostramos algunos ejemplos en Smalltalk, ya que permite la aparición de todas estas características, para dejar más en claro de qué manera podemos identificarlas.

### Ejemplo 1

`Date today`

-   Transparencia Referencial: NO (Con cualquiera de las 2 definiciones de transparencia referencial)
-   Efecto de Lado: NO
-   Asignación Destructiva: NO

Evaluarlo con los mismos parámetros (o sea ninguno) en días distintos va a dar resultados distintos. Reemplazar la operación por el resultado una vez que cambia el día se rompe todo. Asignación destructiva y efecto de lado hay, pero en la CPU que actualiza la variable que indica el tiempo, no en el mensaje `today` que consulta ese valor (no se si es tan así, pero es a modo ilustrativo).

El efecto colateral de otra operación afecta a esta operación y le hace perder la transparencia referencial, a pesar de que esta operación por si misma NO tiene efecto de lado.

Ejemplos como este hacen que transparencia referencial y efecto colateral no sean conceptos opuestos.

### Ejemplo 2

`#LaColeccionConEfectoDeLado`
`>>add: unElemento`
`  "El add: siempre devuelve lo que se agrega. Acá se redefine para avisarle al elemento que fue agregado (no se me ocurrió nada mejor)"`
`  unElemento teAgregaronEn: self.`
`  ^super add: unElemento.`

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

Preguntas frecuentes
--------------------

Qué tal, tengo la siguiente pregunta:

Leí una definición de Transparencia Referencial: “Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto”

Con este criterio, aquí sí habría transparencia referencial:

int a=1;

int c;

c=a++;

Ya que es lo mismo que hacer esto:

int a=1;

int c;

c=1;

Ya que el efecto en la variable c es el mismo: va a valer 1.

Pero para mí no es así, ya que no va a haber transparencia referencial, porque si bien se logra el mismo efecto con respecto a c, el sistema cambia (la variable a se incrementa en una unidad).

¿Es correcto afirmar entonces que si no hay Efecto de Lado entonces tengo garantizada la Transparencia referencial y viceversa?

Atte!

Desde ya, muchas gracias…

\_\_.\_,\_.\_\_\_

Your email settings: Individual Email|Traditional Change settings via the Web (Yahoo! ID required) Change settings via email: Switch delivery to Daily Digest | Switch to Fully Featured Visit Your Group | Yahoo! Groups Terms of Use | Unsubscribe

\_\_,\_.\_,\_\_\_ Reply

Forward

Reply

| Daniel Solmirano

`to pdep`
`   `

show details 4:37 PM (16 hours ago)

Reply

`   Follow up message`

Images are not displayed. Display images below - Always display images from dsolmirano@gmail.com

Santiago,

las definiciones no son intercambiables. Seguramente hay sistemas con efecto de lado Y que además logran transparencia referencial (cualquier sistema donde no se usen variables globales o estáticas, y no tenga funciones de entrada salida). En smalltalk no es dificil de conseguir. En c++ tampoco (siempre que uses const para marcar las cosas que querés que sean inmutables).

Sin embargo, ausencia de efecto de lado implica transparencia referencial, simplemente por el hecho de que el sistema no te permite cambiar el estado de las variables en un contexto dado.

Saludos, Daniel

2009/8/20 Santiago Villarreal &lt;villarrealsantiago@yahoo.com.ar&gt; - Show quoted text -

`   Qué tal, tengo la siguiente pregunta:`

`   Leí una definición de Transparencia Referencial: “Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto”`

`   Con este criterio, aquí sí habría transparencia referencial:`

`   int a=1;`

`   int c;`

`   c=a++;`

`   Ya que es lo mismo que hacer esto:`

`   int a=1;`

`   int c;`

`   c=1;`

`   Ya que el efecto en la variable c es el mismo: va a valer 1.`

`   Pero para mí no es así, ya que no va a haber transparencia referencial, porque si bien se logra el mismo efecto con respecto a c, el sistema cambia (la variable a  se incrementa en una unidad).`

`   ¿Es correcto afirmar entonces que si no hay Efecto de Lado entonces tengo garantizada la Transparencia referencial y viceversa?`

`   Atte!`

`   Desde ya, muchas gracias…`

\_\_.\_,\_.\_\_\_ - Show quoted text -

Your email settings: Individual Email|Traditional Change settings via the Web (Yahoo! ID required) Change settings via email: Switch delivery to Daily Digest | Switch to Fully Featured Visit Your Group | Yahoo! Groups Terms of Use | Unsubscribe

\_\_,\_.\_,\_\_\_ Reply

Forward

Reply by chat to Daniel

Reply

| Santiago Villarreal

`to pdep`
`   `

show details 4:52 PM (16 hours ago)

Reply

`   Follow up message`

Images are not displayed. Display images below - Always display images from villarrealsantiago@yahoo.com.ar

La verdad me confunden los términos de Transparencia con Efecto de Lado, según la definición “Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto”, no me queda claro en dónde se logra el mismo efecto…

En el ejemplo que di entonces no habría transparencia, ya que una vez que si reemplazo a c=a++; por c=1; el sistema se comporta de dos maneras distintas… en el primero “a” cambia de valor, en el segundo no, por lo tanto no se logra el mismo efecto que es hacer valer uno a “c” y luego incrementar “a”.

… es ahí donde se me mezcla la cosa masomenos…

Espero que se entienda!!

Un gran saludo!

De: pdep@yahoogroups.com [1](mailto:pdep@yahoogroups.com) En nombre de Daniel Solmirano Enviado el: Jueves, 20 de Agosto de 2009 04:37 p.m. Para: pdep@yahoogroups.com Asunto: Re: \[pdep\] Transparencia Referencial y Efecto de Lado - Show quoted text -

Santiago,

las definiciones no son intercambiables. Seguramente hay sistemas con efecto de lado Y que además logran transparencia referencial (cualquier sistema donde no se usen variables globales o estáticas, y no tenga funciones de entrada salida). En smalltalk no es dificil de conseguir. En c++ tampoco (siempre que uses const para marcar las cosas que querés que sean inmutables).

Sin embargo, ausencia de efecto de lado implica transparencia referencial, simplemente por el hecho de que el sistema no te permite cambiar el estado de las variables en un contexto dado.

Saludos, Daniel

2009/8/20 Santiago Villarreal &lt;villarrealsantiago@yahoo.com.ar&gt;

Qué tal, tengo la siguiente pregunta:

Leí una definición de Transparencia Referencial: “Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto”

Con este criterio, aquí sí habría transparencia referencial:

int a=1;

int c;

c=a++;

Ya que es lo mismo que hacer esto:

int a=1;

int c;

c=1;

Ya que el efecto en la variable c es el mismo: va a valer 1.

Pero para mí no es así, ya que no va a haber transparencia referencial, porque si bien se logra el mismo efecto con respecto a c, el sistema cambia (la variable a se incrementa en una unidad).

¿Es correcto afirmar entonces que si no hay Efecto de Lado entonces tengo garantizada la Transparencia referencial y viceversa?

Atte!

Desde ya, muchas gracias…

\_\_.\_,\_.\_\_\_

Your email settings: Individual Email|Traditional Change settings via the Web (Yahoo! ID required) Change settings via email: Switch delivery to Daily Digest | Switch to Fully Featured Visit Your Group | Yahoo! Groups Terms of Use | Unsubscribe

\_\_,\_.\_,\_\_\_ Reply

Forward

Reply

| Germán Leiva

`to pdep`
`   `

show details 6:45 PM (14 hours ago)

Reply

`   Follow up message`

Images are not displayed. Display images below - Always display images from leivagerman@gmail.com

Vamos con un ejemplo

Imaginate que tenés una clase Agenda en Smalltalk con los siguiente métodos

Agenda &gt;&gt; diaDeHoy

`     ^Date today`

Agenda &gt;&gt; colorDelDiaDeHoy

`     ^self diaDeHoy esFeriado`
`           ifTrue: [ ^Color new: #verde ]`
`           ifFalse: [ ^Color new: #rojo ].`

"Un ejemplo re pedorro ya lo se"

Cambiar la línea que está subrayada por (Date newDay: 20 monthIndex: 8 year: 2009) o sea, una instancia de Date que representa el 20 de agosto de 2009 - el resultado de la operación -, solamente va a hacer que el programa "funcione bien" hoy.

Analizando esto con la definición de transparencia referencial “Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto”

Podemos ver que reemplazar el mensaje diaDeHoy por su resultado NO no va a tener el mismo efecto.

Analizando el el efecto de lado, podemos ver que el mensaje diaDeHoy no altera el estado de nada (tanto fuera del método como dentro).

En este ejemplo en particular, la operación diaDeHoy no tiene transparencia referencial y no tiene efecto de lado

Ahora bien, si una operación tiene efecto de lado seguro que no tiene transparencia referencial

Espero que se haya entendido

Saludos,

El 20 de agosto de 2009 16:52, Santiago Villarreal &lt;villarrealsantiago@yahoo.com.ar&gt; escribió: - Show quoted text -

`   La verdad me confunden los términos de Transparencia con Efecto de Lado, según la definición “Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto”, no me queda claro en dónde se logra el mismo efecto…`

`   En el ejemplo que di entonces no habría transparencia, ya que una vez que si reemplazo a c=a++; por c=1; el sistema se comporta de dos maneras distintas… en el primero “a” cambia de valor, en el segundo no, por lo tanto no se logra el mismo efecto que es hacer valer uno a “c” y luego incrementar “a”.`

`   … es ahí donde se me mezcla la cosa masomenos…`

`   Espero que se entienda!!`

`   Un gran saludo!`

`   De: pdep@yahoogroups.com `[`2`](mailto:pdep@yahoogroups.com)` En nombre de Daniel Solmirano`
`   Enviado el: Jueves, 20 de Agosto de 2009 04:37 p.m.`
`   Para: pdep@yahoogroups.com`
`   Asunto: Re: [pdep] Transparencia Referencial y Efecto de Lado`

`   Santiago,`

`   las definiciones no son intercambiables. Seguramente hay sistemas con efecto de lado Y que además logran transparencia referencial (cualquier sistema donde no se usen variables globales o estáticas, y no tenga funciones de entrada salida). En smalltalk no es dificil de conseguir. En c++ tampoco (siempre que uses const para marcar las cosas que querés que sean inmutables).`

`   Sin embargo, ausencia de efecto de lado implica transparencia referencial, simplemente por el hecho de que el sistema no te permite cambiar el estado de las variables en un contexto dado.`

`   Saludos,`
`   Daniel`

`   2009/8/20 Santiago Villarreal <villarrealsantiago@yahoo.com.ar>`

`   Qué tal, tengo la siguiente pregunta:`

`   Leí una definición de Transparencia Referencial: “Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto”`

`   Con este criterio, aquí sí habría transparencia referencial:`

`   int a=1;`

`   int c;`

`   c=a++;`

`   Ya que es lo mismo que hacer esto:`

`   int a=1;`

`   int c;`

`   c=1;`

`   Ya que el efecto en la variable c es el mismo: va a valer 1.`

`   Pero para mí no es así, ya que no va a haber transparencia referencial, porque si bien se logra el mismo efecto con respecto a c, el sistema cambia (la variable a  se incrementa en una unidad).`

`   ¿Es correcto afirmar entonces que si no hay Efecto de Lado entonces tengo garantizada la Transparencia referencial y viceversa?`

`   Atte!`

`   Desde ya, muchas gracias…`

--

Germán Leiva leivagerman@gmail.com

\_\_.\_,\_.\_\_\_ - Show quoted text -

Your email settings: Individual Email|Traditional Change settings via the Web (Yahoo! ID required) Change settings via email: Switch delivery to Daily Digest | Switch to Fully Featured Visit Your Group | Yahoo! Groups Terms of Use | Unsubscribe

\_\_,\_.\_,\_\_\_ Reply

Forward

Germán is not available to chat

Reply

| Carla F. Griggio

`to pdep`
`   `

show details 1:32 AM (7 hours ago)

Reply

`   Follow up message`

Images are not displayed. Display images below - Always display images from carla.griggio@gmail.com

`       En el ejemplo que di entonces no habría transparencia, ya que una vez que si reemplazo a c=a++; por c=1; el sistema se comporta de dos maneras distintas… en el primero “a” cambia de valor, en el segundo no, por lo tanto no se logra el mismo efecto que es hacer valer uno a “c” y luego incrementar “a”.`

Quizás me equivoque pero... no se logra el mismo efecto porque no estás haciendo lo mismo. En el segundo caso a no cambia de valor porque no estás usando a. Obvio que no va a cambiar de valor! Creo que estás comparando dos situaciones que van para lados diferentes.

Creo que una situación similar que sí es comparable es esta:

a=1; c= ++a; Entonces c es 2, porque ++a da 2.

Si hacemos

c=2

Que sería reemplazar a la operación ++a por su resultado, llegamos a lo mismo (c es 2).

Como decían los muchachos, cuando no hay efecto de lado hay transparencia referencial porque el estado del sistema no cambia, entonces una operación con los mismos "operandos" siempre va a dar igual. Cuando hay efecto de lado no se puede asegurar la transparencia referencial, porque puede pasar que la operación por la que estás reemplazando su resultado, no de siempre el mismo resultado, sin embargo en algún contexto podría encontrarse transparencia referencial.

Por ahí, a la definición que tenés de transparencia referencial, está bueno agregarle la palabra que pongo en negrita:

“Hay transparencia referencial si al reemplazar una operación por su resultado siempre se logra el mismo efecto”

(Leer "siempre" con el sentido temporal, claro, no con el sentido "en todas las situaciones").

\_\_,\_.\_,\_\_

\_\_.\_,\_.\_\_\_ - Show quoted text -

Your email settings: Individual Email|Traditional Change settings via the Web (Yahoo! ID required) Change settings via email: Switch delivery to Daily Digest | Switch to Fully Featured Visit Your Group | Yahoo! Groups Terms of Use | Unsubscribe

\_\_,\_.\_,\_\_\_ Reply

Forward

Carla is not available to chat

Reply

| Nicolas Passerini

`to pdep`
`   `

show details 8:53 AM (3 minutes ago)

Reply

`   Follow up message`

`   En el ejemplo que di entonces no habría transparencia, ya que una vez que si reemplazo a c=a++; por c=1; el sistema se comporta de dos maneras distintas… en el primero “a” cambia de valor, en el segundo no, por lo tanto no se logra el mismo efecto que es hacer valer uno a “c” y luego incrementar “a”.`

En el ejemplo que vos diste no hay transparencia referencial, es correcta la interpretación. La expresión a++ tiene el efecto colateral de modificar el valor de a, por lo tanto no puede tener transparencia referencial. Reply

Forward

`   Your message has been sent. Invite pdep to Gmail    `
`       `

Reply

| Nicolas Passerini

`to pdep`
`   `

show details 8:56 AM (0 minutes ago)

Reply

`   Follow up message`

Tal vez la duda viene por la idea de efecto.

El efecto no es solamente el resultado. El efecto incluye todas las posibles consecuencias de evaluar una expresión, en particular lo que llamamos efecto de lado (o mejor dicho colateral).

Entonces si hay efecto colateral, hay efecto (y no puede haber transparencia referencial).

2009/8/21 Nicolas Passerini &lt;npasserini@gmail.com&gt;

`       En el ejemplo que di entonces no habría transparencia, ya que una vez que si reemplazo a c=a++; por c=1; el sistema se comporta de dos maneras distintas… en el primero “a” cambia de valor, en el segundo no, por lo tanto no se logra el mismo efecto que es hacer valer uno a “c” y luego incrementar “a”.`

`   En el ejemplo que vos diste no hay transparencia referencial, es correcta la interpretación.`
`   La expresión a++ tiene el efecto colateral de modificar el valor de a, por lo tanto no puede tener transparencia referencial.`
