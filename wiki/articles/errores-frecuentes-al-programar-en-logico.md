### Errores conceptuales más importantes

-   Estilos de programación no *lógicos*, por ejemplo:
    -   Utilización innecesaria de listas.
    -   Estrategias algorítmicas.
-   Programación "con efectos colaterales", por ejemplo intentos de "asignar" más de una vez un valor una misma variable.
-   Falta de abstracción, que puede verse reflejada en:
    -   Uso de trucos programáticos de bajo nivel, en lugar de representaciones abstractas de alto nivel.
    -   Ausencia de predicados auxiliares que abstraigan conceptos reutilizables en diferentes predicados de alto nivel (lo que normalmente lleva a la repetición de código).
    -   Malas deciciones a la hora de separar un predicado en subtareas, que llevan a tener predicados auxiliares que no representan abstracciones o conceptos útiles.
-   Problemas de inversibilidad; ya sea por falta de generación o por hacerlo incorrecta-/innecesariamente.
-   Confundir predicados con funciones.

### Problemas generales de programación

-   Incumplimiento de las consignas.
-   Código que no se entiende, desprolijo, desordenado.
-   Inconsistencias en general, por ejemplo:
    -   Las diferentes reglas de un predicado esperan parámetros con significados distintos.
    -   Una misma variable utilizada con diferentes objetivos inconsistentemente.

### Errores más técnicos

-   Mal uso del pattern matching, en dos versiones:
    -   No aprovecharlo para quedarse con una parte de una estructura más grande (functor o lista).
    -   Usar patrones demasiado específicos, perdiendo la oportunidad de construir predicados polimórficos.
-   Usar igualdad (**=**) en lugar de **is**.
-   Usar el símbolo **=** en lugar de la misma variable.

### Cuestiones de estilo

-   No elegir buenos nombres para variables y/o predicados.
-   Utilizar pasos intermedios innecesarios que no aportan a la claridad del código.
-   No utilizar variables anónimas donde corresponde.
-   Complejidad innecesaria.

