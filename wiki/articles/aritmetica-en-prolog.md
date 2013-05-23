El predicado "is"
-----------------

En Prolog no pasa como en Haskell que tengo funciones que me devuelven cosas, sino que tengo predicados que relacionan individuos ¿Entonces qué sucede cuando quiero hacer cuentas? ¿Y como me puedo “guardar” el resultado de una cuenta? Recordemos que en Prolog no existe el concepto de asignación.

Bueno, para esos casos, existe el predicado is. Es un predicado un poco especial (ya que no se escribe como todos los demás). Una consulta que podemos hacer usando is podría ser:

`?- 3 is 2 + 1.`
`Yes`

A la derecha del is se escribe una operación aritmética. A la izquierda del is se escribe el resultado de esa operación aritmética.

El is **sólo es inversible por el primer parámetro**

`?- X is 6 / 2.`
`X = 3`

Errores comunes con el is
-------------------------

Acá vienen una serie de warnings que deben tener MUY en cuenta:

===Error: usar = en vez de is=== En general en la materia nunca vamos a usar el = ya que preferimos el uso de pattern matching y usarlo para resolver operaciones aritméticas no es correcto. Por qué no se puede usar = para aritmética? Veamos un ejemplo:

`?- 3+5 = 2+6.`
`No`
`?- 3+5 = 5+3.`
`No`
`?- 3+5 = 8.`
`No`

El muy simpático sólo nos va a decir true cuando las dos expresiones de ambos lados del igual sean idénticas, no va a intentar resolver la igualdad:

`?- 3+5 = 3+5.`
`Yes`

Muy útil, no? :P

### Error: tratar de acumular

-   No usar el is como asignación. Como ya saben, en lógico no hay asignación, una vez que las variables se ligan, permanecen ligadas hasta que termine la consulta. Es decir, no vale preguntar algo como:

`?- edad(pepe,E), E is E + 1.`
`No`

No existe ningún número E que sea igual a E + 1. Es equivalente a preguntar si:

`?- edad(pepe,E), E = E + 1.`
`No`

Miremos otras consultas:

`?- 3 is 3 + 1.`
`No`
`?- 131 is 131 + 1.`
`No`
`?- 0 is 0 + 1.`
`No`

### Error: Usar is para igualar variables

El is es **sólo para operaciones aritméticas**. Si bien funciona, lo siguiente es un error conceptual:

`hermano(PersonaA,PersonaB):-`
`        padre(PadreA,PersonaA),`
`        padre(PadreB,PersonaB),`
`        PadreA is PadreB`

¡Error conceptual! Lo mismo sucede aquí:

`hermano(PersonaA,PersonaB):-`
`        padre(PadreA,PersonaA),`
`        padre(PadreB,PersonaB),`
`        PadreA = PadreB`

La manera correcta de hacerlo es:

`hermano(PersonaA,PersonaB):-`
`        padre(Padre,PersonaA),`
`        padre(Padre,PersonaB).`

Porque si dos individuos deben ser el mismo, entonces debemos escribirlos con la misma variable, no como otra condición. Prolog solito se dará cuenta. Vemos que de ésta manera mejoramos la **declaratividad** (leo qué es lo que quiero, y hay menos detalles algorítmicos: si el padre es el mismo lo escribo igual, no agrego una condición). El motor de Prolog se encarga del resto.

### Error: Usar is para resolver ecuaciones

is **no es completamente inversible**, es inversible solo para el primer argumento, lo que va a la izquierda. O sea, no vale preguntar:

`?- 3 is X + 1.`

Porque el motor de Prolog no sabe resolver ecuaciones. En éste caso, el error dirá algo como "Arguments are not sufficiently instanciated", lo cual es otra manera de decir "flaco, no soy inversible a derecha, sólo me podés poner algo sin unificar a izquierda"
