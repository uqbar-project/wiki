Memoria compartida (*shared memory*)
------------------------------------

La memoria compartida es un espacio común de datos en el que múltiples componentes (por ejemplo: procedimientos) pueden leer y escribir información. Implementaciones típicas de memoria compartida son las variables globales o las bases de datos como herramienta de integración entre componentes.

`Stack s = ;`

En el caso general la memoria compartida representa un grado alto de acoplamiento ya que

Call & Return
-------------

`#include <stdio.h>`
`#include <stdlib.h>`
`#define SIZE 50`
`void push(int i);`
`int pop(void);`
`int  *tos, *p1, stack[SIZE];`
`int main(void)`
`{`
`  int value;`
`  tos = stack; /* tos points to the top of stack */`
`  p1 = stack; /* initialize p1 */`
`  do {`
`    printf("Enter value: ");`
`    scanf("%d", &value); `
`    if(value != 0) push(value);`
`    else printf("value on top is %d\n", pop());`
`  } while(value != -1);`
`  return 0;`
`} `
`void push(int i)`
`{`
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

Excepciones
-----------

Continuations
-------------

<http://en.wikipedia.org/wiki/Continuation-passing_style> <http://en.wikipedia.org/wiki/Callback_(computer_science)>

Eventos
-------

Mensajes asincrónicos
---------------------
