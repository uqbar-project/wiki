### Descripción del problema

Vamos a estudiar éstos casos:

1.  "Dos personas son hermanastros cuando tienen igual padre y diferente madre"
2.  "

### El is sólo se usa para cuentas, las cuentas sólo se hacen con is

Ésta solución **está mal**:

`hermanastros(Hermano1,Hermano2):-`
`   padre(Hermano1,Padre1),`
`   padre(Hermano2,Padre2),`
`   madre(Hermano1,Madre1),`
`   madre(Hermano2,Madre2),`
`   Padre1 is Padre2,`
`   Madre1 \= Madre2.`

Ésta solución también **está mal**:

`hacerrrrrrrrr`

Éste título significa, por un lado, que

-   No vale hacer <algo> `is` `Variable.`
-   No vale hacer <algo> `is` `4.`

Si bien funciona, debemos usar las herramientas para lo que corresponde. Cuando una persona lee un is, espera cuentas. Debemos usar las abstracciones que comuniquen correctamente nuestras ideas.

Por otro lado, lo siguiente directamente no funciona:

-   No vale hacer `Variable` `=` `5` `*` `3.`
-   No vale hacer `factorial(N+1,Fac).` (se debe hacer `Siguiente` `is` `N+1,` `factorial(Siguiente,Fac).` )

Entonces:

-   **Sí** vale `Algo` `is` `Variable` `+` `1.` (Siempre que Variable venga unificada).

Más abajo la solución certera. === Variable = OtraVariable === Ésta solución **está mal**:

`hermanastros(Hermano1,Hermano2):-`
`   padre(Hermano1,Padre1),`
`   padre(Hermano2,Padre2),`
`   madre(Hermano1,Madre1),`
`   madre(Hermano2,Madre2),`
`   Padre1 = Padre2,`
`   Madre1 \= Madre2.`

Ésto no significa que el igual esté prohibido. Ver los casos abajo de todo.

=== Variable = individuo ===

Ésto no significa que el igual esté prohibido. Ver los casos abajo de todo.

=== Casos en los que el = es correcto ===

### Conclusión

El igual es necesario en contados casos. La mayoría de las veces uno se puede arreglar con la metáfora de identidad de lógico, y con un poquito de unificación y pattern matching.
