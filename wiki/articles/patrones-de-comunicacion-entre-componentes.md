Memoria compartida (*shared memory*)
------------------------------------

La memoria compartida es un espacio común de datos en el que múltiples componentes (por ejemplo: procedimientos) pueden leer y escribir información. Implementaciones típicas de memoria compartida son las variables globales o las bases de datos como herramienta de integración entre componentes.

### Ejemplo

A continuación podemos ver un ejemplo de una pila programada en C utilizando esta forma de compartir información:

` #include <stdio.h>`
` #define SIZE 50`
` `
` int  current, stack[SIZE];`
` `
` void init() {`
`   current = 0; /* initialize current */`
` }`
` `
` void push(int i) {`
`   current++;`
`   if(current == (SIZE)) {`
`     printf("Stack Overflow.\n");`
`     exit(1);`
`   }`
`   stack[current] = i;`
` }`
` `
` int pop(void) {`
`   if(current == 0) {`
`     printf("Stack Underflow.\n");`
`     exit(1);`
`   }`
`   current--;`
`   return stack[current+1];`
` }`
` `
` int main(void) {`
`   int value;`
`   init();`
`   `
`   push(1);`
`   push(2);`
`   printf("First value on top is %d\n", pop());`
`   printf("Second value on top is %d\n", pop());`
`   printf("Third value on top is %d\n", pop());`
`   printf("end\n");`
`       `
`   return 0;`
` }`

### Consecuencias

Algunas consecuencias de esta forma de compartir información son:

-   No hay un mecanismo sencillo para saber qué procedimientos incluyen referencias a las variables globales y
-   No es posible tener dos pilas simultáneamente, en caso de intentarlo se mezclaría la información de ambas.
-   La modificación de cualquiera de las operaciones que acceden a la estructura de datos implica revisar su buen comportamiento en relación con todas las demás (que, como se dijo antes, puede no ser posible saber exactamente cuáles son). Lo mismo ocurre si se desea modificar la estructura de los datos compartidos.

En el caso general la memoria compartida representa un grado alto de acoplamiento ya que a priori no es posible saber qué componentes modifican o leen qué parte de los datos. En sistemas donde la información compartida está sostenida en un motor de bases de datos (por ejemplo RDBMS), estos pueden mitigar parcialmente este problema al implementar esquemas de seguridad que restrinjan en parte el acceso a los datos a sólo la parte necesaria de cada componente.

Call & Return
-------------

En el mecanismo de call & return la comunicación se da entre un componente invocante o llamador (*caller*) que invoca o referencia a otro componente invocado o llamado (*callee*). Si bien el flujo de información es bidireccional, es asimétrico:

-   El componente invocado puede recibir *parámetros*, que le permiten al invocante transferirle información mediante *argumentos*.
-   En el caso general el componente invocado puede producir un *valor de retorno*, mediante este mecanismo se logra la bidireccionalidad de la comunicación. En algunas tecnologías pueden existir limitaciones que restringen el uso de valores de retorno, en ese caso la comunicación será unidireccional.

El ejemplo más preciso de esta idea se encuentra en los lenguajes funcionales puros, es decir, sin la posibilidad de [ efecto](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html). Podemos ver un ejemplo de implementación de una pila en Haskell:

### Consecuencias

-   Si bien la comunicación puede ser bidireccional, el conocimiento (y por lo tanto el acoplamiento) es *a priori* unidireccional, es decir, el componente llamado no tiene ningún conocimiento de el origen del mensaje y aún puede devolver información sin tener conocimiento del destino de la misma.

Excepciones
-----------

Continuations
-------------

<http://en.wikipedia.org/wiki/Continuation-passing_style> <http://en.wikipedia.org/wiki/Callback_(computer_science)>

Eventos
-------

Mensajes asincrónicos
---------------------
