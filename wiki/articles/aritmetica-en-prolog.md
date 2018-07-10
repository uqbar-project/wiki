---
layout: article
title: Aritmetica en prolog
---

## El predicado "is"

En Prolog no pasa como en Haskell que tengo funciones que me devuelven cosas, sino que tengo predicados que relacionan individuos ¿Entonces qué sucede cuando quiero hacer cuentas? ¿Y como me puedo “guardar” el resultado de una cuenta? Recordemos que en Prolog no existe el concepto de asignación.

Bueno, para esos casos, existe el predicado is. Es un predicado un poco especial (ya que no se escribe como todos los demás). Una consulta que podemos hacer usando is podría ser:

```Prolog
?- 5 is 2 + 3.
Yes
```

Pero **ojo**, la cuenta sólo puede ir a la derecha:

```Prolog
?- 4 + 1 is 2 + 3.
No.
```

A la derecha del is se escribe una operación aritmética. A la izquierda del is se escribe el resultado de esa operación aritmética.

El is **sólo es inversible por el primer parámetro**

Esto funciona:

```Prolog
?- X is 6 / 2.
X = 3
```

Pero esto no:

```Prolog
?- 3 is X / 2.
ERROR: is/2: Arguments are not sufficiently instantiated
```

## Errores comunes con el is

Acá vienen una serie de warnings que deben tener MUY en cuenta:

### Error: usar = en vez de is

En general en la materia nunca vamos a usar el = ya que preferimos el uso de pattern matching y usarlo para resolver operaciones aritméticas no es correcto. ¿Por qué no se puede usar = para aritmética? Veamos un ejemplo:

```Prolog
?- 3+5 = 2+6.
No
?- 3+5 = 5+3.
No
?- 3+5 = 8.
No
```

El muy simpático sólo nos va a decir true cuando las dos expresiones de ambos lados del igual sean idénticas, no va a intentar resolver la igualdad:

```Prolog
?- 3+5 = 3+5.
Yes
```

Muy... "útil", ¿no? :P

### Error: tratar de acumular

Como ya saben, en lógico no hay asignación, una vez que las variables se ligan, permanecen ligadas hasta que termine la consulta, por ende no hay que pensar al is como un mecanismo para asignar. Es decir, no vale preguntar algo como:

```Prolog
?- edad(pepe,E), E is E + 1.
No
```

No existe ningún número E que sea igual a E + 1. Es equivalente a preguntar si:

```Prolog
?- edad(pepe,E), E = E + 1.
No
```

Básicamente lo que estaría pasando es esto:

```Prolog
?- 3 is 3 + 1.
No
?- 0 is 0 + 1.
No
```

### Error: Usar is para verificar igualdad de valores

El is es **sólo para operaciones aritméticas**. Si bien funciona, lo siguiente es un error conceptual:

```Prolog
edad(juan,Edad) :- 
  Edad is 10.
```
La manera correcta de hacerlo es aprovechando que al hacer pattern matching, prolog *verifica que el valor sea ese*, sin usar el igual ó el is:

`edad(juan,10).`

¡Y listo!

Ver [Sobre el uso del igual en Prolog](sobre-el-uso-del-igual-----en-prolog.html) para más información.

### Error: Usar is para igualar variables

Esto es el mismo caso que el error anterior. El is es **sólo para operaciones aritméticas**. Si bien funciona, lo siguiente es un error conceptual:

```Prolog
mismaEdad(PersonaA,PersonaB):-
  edad(PersonaA,EdadA),
  edad(PersonaB,EdadB),
  EdadA is EdadB
```

Lo mismo sucede aquí:

```Prolog
mismaEdad(PersonaA,PersonaB):-
  edad(PersonaA,EdadA),
  edad(PersonaB,EdadB),
  EdadA = EdadB
```

La manera correcta de hacerlo es:

```Prolog
mismaEdad(PersonaA,PersonaB):-
  edad(PersonaA,Edad),
  edad(PersonaB,Edad).
```

Porque si dos individuos deben ser el mismo, entonces alcanza con escribirlos con la misma variable, no como otra condición. De esta forma estamos haciendo una **consulta existencial** respecto a la edad de la PersonaA y luego una **consulta individual** para validar si es cierto que la edad de PersonaB es esa que ya conocemos. Vemos que de ésta manera mejoramos la **declaratividad** (leo qué es lo que quiero, y hay menos detalles algorítmicos: si la edad es la misma lo escribo igual y listo). El motor de Prolog se encarga del resto.

Ver [Sobre el uso del igual en Prolog](sobre-el-uso-del-igual-----en-prolog.html) para más información.

### Error: Usar is para resolver ecuaciones

Como dijimos arriba, el predicado is/2 **no es completamente inversible**, es inversible solo para el primer argumento, lo que va a la izquierda. O sea, no vale preguntar:

```Prolog
?- 3 is X + 1.
ERROR: is/2: Arguments are not sufficiently instantiated
```

Porque el motor de Prolog no sabe resolver ecuaciones. En éste caso, el error dirá algo como "Arguments are not sufficiently instantiated", lo cual es otra manera de decir "No soy inversible a derecha, sólo me podés poner algo sin unificar a izquierda"
