Memoria compartida (*shared memory*)
------------------------------------

La memoria compartida es un espacio común de datos en el que múltiples componentes (por ejemplo: procedimientos) pueden leer y escribir información. Implementaciones típicas de memoria compartida son las variables globales o las bases de datos como herramienta de integración entre componentes.

En el caso general la memoria compartida representa un grado alto de acoplamiento ya que a priori no es posible saber qué componentes modifican o leen qué parte de los datos. En sistemas donde la información compartida está sostenida en un motor de bases de datos (por ejemplo RDBMS), estos pueden mitigar parcialmente este problema al implementar esquemas de seguridad que restrinjan en parte el acceso a los datos a sólo la parte necesaria de cada componente.

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

Algunas consecuencias de esta forma de compartir información son:

-   No hay un mecanismo sencillo para saber qué procedimientos incluyen referencias a las variables globales {{code|current} y stack

Call & Return
-------------

Excepciones
-----------

Continuations
-------------

<http://en.wikipedia.org/wiki/Continuation-passing_style> <http://en.wikipedia.org/wiki/Callback_(computer_science)>

Eventos
-------

Mensajes asincrónicos
---------------------
