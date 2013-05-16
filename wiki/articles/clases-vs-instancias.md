En los lenguajes orientados a objetos que tienen clases (como Java, Xtend, Groovy, Scala, Smalltalk, Ruby, etc.) es frecuente como diseñadores preguntarnos si un determinado requerimiento se puede resolver con instancias o con clases.

**Ejemplo:** tenemos tareas en un proyecto, a cada tarea le corresponden distintos impuestos

-   el impuesto A es un 3% del costo de la tarea
-   el impuesto B representa un 2% del costo de la tarea
-   el impuesto C es un 1,5% del costo de la tarea

Resolvemos el valor del impuesto A

<code>

metodo valor(Tarea tarea)

`   tarea.costo * 0.03`

fin

</code>

Y a continuación el valor del impuesto B

<code>

metodo valor(Tarea tarea)

`   tarea.costo * 0.02`

fin

</code>

Pero si analizamos con detenimiento veremos que los 3 impuestos comparten el mismo cálculo: la única diferencia es el % que se le aplica al costo de una tarea. No tiene sentido armar una jerarquía de clases para los impuestos:

-   Impuesto

`* Impuesto A, método valor(Tarea)`
`* Impuesto B, método valor(Tarea)`
`* Impuesto C, método valor(Tarea)`

Es mucho más conveniente generar una única abstracción impuesto que tenga el porcentaje como atributo:

<code>

Clase Impuesto

atributo porcentaje

metodo valor(Tarea tarea)

`   tarea.costo * porcentaje`

fin

</code>

Y no necesitamos tener las clases Impuesto A, B y C: si existieran, sólo se diferenciarian en el valor que almacenan en el porcentaje al construirse.

**Conviene trabajar con clases cuando hay comportamiento diferencial entre ellos**, por el contrario cuando no hay diferencias en el comportamiento es preferible modelar esa solución con objetos. Una clase que no define comportamiento o atributos es sospechosa y como diseñadores deberíamos justificar una abstracción de esta naturaleza.

Si apareciera un Impuesto D, que se calcula como el máximo entre $ 400 y el 5% del costo de la tarea, entonces sí tendría justificativo crear una nueva clase para modelar este impuesto, dado que el cálculo es diferente.
