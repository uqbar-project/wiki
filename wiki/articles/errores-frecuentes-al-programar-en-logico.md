---
layout: article
title: Errores frecuentes al programar en logico
---

### Errores conceptuales más importantes

-   Confundir los conceptos del paradigma, por ejemplo:
    -   predicados con funciones - los predicados no devuelven cosas, tienen valor de verdad. Tenerlo en cuenta para la cantidad de parámetros y para no consultarlos mal
    -   variables con individuos (en particular átomos) - sólo escriban variables con mayúscula!
    -   [predicados con individuos](errores-comunes--usar-un-predicado-como-si-fuera-una-variable.html)
-   Errores lógicos propiamente dichos:
    -   Confundir [**Y** con **O**](paradigma-logico---conjuncion-y-disyuncion.html)
    -   [Mezclar antecedente y consecuente en un **forall**](paradigma-logico---el-forall.html)
    -   Confundir *existe* con *para todo* (forall).
-   Estilos de programación no *lógicos*, por ejemplo:
    -   [Utilización innecesaria de listas](paradigma-logico---listas-errores-comunes--findall-y-member.html).
    -   Estrategias algorítmicas.
-   Programación "con efectos colaterales", por ejemplo intentos de "asignar" más de una vez un valor una misma variable.
-   Falta de abstracción, que puede verse reflejada en:
    -   Uso de trucos programáticos de bajo nivel, en lugar de representaciones abstractas de alto nivel.
    -   Ausencia de predicados auxiliares que abstraigan conceptos reutilizables en diferentes predicados de alto nivel (lo que normalmente lleva a la repetición de código).
    -   Malas deciciones a la hora de separar un predicado en subtareas, que llevan a tener predicados auxiliares que no representan abstracciones o conceptos útiles.
-   [Problemas de inversibilidad](paradigma-logico---casos-de-no-inversibilidad.html); ya sea por falta de generación o por hacerlo incorrecta-/innecesariamente.

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
-   Usar igualdad (**=**) en lugar de **is**. -&gt; Explicación: el = no resuelve cuentas. Es la igualdad más trivial, la igualdad visual y directa de patrones y átomos.
-   [ Usar el símbolo **=** en lugar de la misma variable, ó del individuo correspondiente](sobre-el-uso-del-igual-----en-prolog.html).

### Cuestiones de estilo

-   No elegir buenos nombres para variables y/o predicados.
-   Utilizar pasos intermedios innecesarios que no aportan a la claridad del código.
-   No utilizar variables anónimas donde corresponde.
-   Complejidad innecesaria.

### Carteles de Error del SWI-Prolog

-   [Warning: Singleton Variables](warning--singleton-variables.html)

