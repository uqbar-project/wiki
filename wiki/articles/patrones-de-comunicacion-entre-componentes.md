Memoria compartida (*shared memory*)
------------------------------------

La memoria compartida es un espacio común de datos en el que múltiples componentes (por ejemplo: procedimientos) pueden leer y escribir información. Implementaciones típicas de memoria compartida son las variables globales o las bases de datos como herramienta de integración entre componentes. A continuación podemos ver un ejemplo de una pila programada en C utilizando esta forma de compartir información:

`#define SIZE 50`
`int  *tos, *p1, stack[SIZE];`

`void init() {`
`  tos = stack; /* tos points to the top of stack */`
`  p1 = stack; /* initialize p1 */`
`}`
`void push(int i) {`
`  p1++;`
`  if(p1 == (tos+SIZE)) {`
`    printf("Stack Overflow.\n");`
`    exit(1);`
`  }`
`  *p1 = i;`
`}`

`int pop(void)`
`{`
`  if(p1 == tos) {`
`    printf("Stack Underflow.\n");`
`    exit(1);`
`  }`
`  p1--;`
`  return *(p1+1);`
`}`

`int main(void)`
`{`
`  int value;`
`  init();`
`  do {`
`    printf("Enter value: ");`
`    scanf("%d", &value); `
`    if(value != 0) push(value);`
`    else printf("value on top is %d\n", pop());`
`  } while(value != -1);`
`  return 0;`
`} `

En el caso general la memoria compartida representa un grado alto de acoplamiento ya que a priori no es posible saber qué componentes modifican o leen qué parte de los datos. En sistemas donde la información compartida está sostenida en un motor de bases de datos (por ejemplo RDBMS), estos pueden mitigar parcialmente este problema al implementar esquemas de seguridad que restrinjan en parte el acceso a los datos a sólo la parte necesaria de cada componente.

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
