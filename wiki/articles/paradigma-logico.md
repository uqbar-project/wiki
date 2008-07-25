### ¿Por qué lo de “lógico”?

Porque está relacionado con conceptos de lógica matemática, en particular:

-   predicado
-   sintaxis
-   fórmula
-   razonamiento
-   deducción

Es un paradigma [declarativo](declaratividad.html) No hay [asignaciones destructivas](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html), se trabaja con el concepto de [Unificación](unificacion.html)

-   [Paradigma Lógico - un poco de nomenclatura](paradigma-logico---un-poco-de-nomenclatura.html)

En este apunte vamos a usar el lenguaje Prolog, que es (en una primera visión) una restricción del lenguaje de la lógica de predicados con una sintaxis distinta. Para dar una introducción intuitiva a la sintaxis de Prolog, traduzcamos el ejemplo de Sócrates. mortal(X):- hombre(X). hombre(socrates).

introducir el concepto de cláusula, y contar lo de cabeza y cuerpo. distinguir hechos de reglas. contar que en el cuerpo pueden ir varias condiciones, separadas por comas que representan un “y”. contar que se le pueden hacer consultas (quedarnos con las individuales) … Entonces, ¿qué es un programa lógico? Un programa lógico es un conjunto de definiciones de predicados. Cada definición puede tener varias cláusulas. Meter otro ejemplo y comentarlo un poco Condiciones: “y” vs “o” Esta sección es para que detecten cuándo es “y” y cuándo es “o”, separando en cláusulas cuando es “o” y poniendo todo en la misma cuando es “y”.

#### Negación

### Algunas Características Relevantes

El paradigma lógico trabaja con el principio de [Universo Cerrado](universo-cerrado.html).

Existen dos tipos de consulta:

;Individuales:Se hacen sobre un individuo específico.<code>

    ?- mortal(socrates).

</code>

Existenciales:Se busca algún individuo que satisfaga la relación. También llamadas variables.  

Consultas existenciales e inversibilidad de predicados 3 Variables ligadas y no ligadas – problemas con la inversibilidad 3 Generadores 3 Consultas existenciales e inversibilidad de predicados Contar que

-   una consulta puede tener [múltiples respuestas](multiples-respuestas.html)
-   la [inversibilidad](paradigma-logico---inversibilidad.html) se puede dar para predicados monádicos también
-   la inversibilidad de un predicado se puede analizar argumento x argumento, decimos que es “inversible” a secas cuando es inversible para todos sus argumentos.
-   No todos los predicados son inversibles, veremos casos de predicados que no resultan inversibles, y también una forma de solucionarlos para que los predicados sí queden inversibles.

Negación

Variables ligadas y no ligadas – problemas con la inversibilidad

La [inversibilidad](paradigma-logico---inversibilidad.html) es la característica que blah, pero [no siempre](paradigma-logico---casos-de-no-inversibilidad.html) funca y en esos casos lo resolvemos con [generación](paradigma-logico---generacion.html)

#### Generación

Contar cómo arreglan el problema de la inversibilidad

### Manejo de conjuntos

forall – “a todos los que les pasa A, les pasa B” • Tener claro si uno quiere poner “uno” o “todos”, si es “todos” va forall, sino va simplemente la condición. Relacionar con cómo se ligan las variables. • Acá también hay problemas con la inversibilidad si ciertas variables no llegan ligadas. Solución con generadores. listas – conjuntos explícitos • Definición de lista: lista vacía o (cabeza y cola) • Pattern matching • Definiciones recursivas • findall o usarlo sólo si lo necesito todo esto con ejemplos de uso

### Tipos de datos en Prolog

Para qué sirven (los 2 casos) Pattern matching con functores – cómo acortan los programas

### Manejo de la Información

Meter acá lo de cómo hacer jugar el estado en el dominó. Sería ideal contar un poco de cómo representar info, p.ej. no ser “listeros”, después si necesito “a todos juntos” tengo el findall. También se puede meter un ejemplo en el que sí convenga modelar con listas, porque los elementos deben estar ordenados. P.ej. el del subte.

-   [Paradigma Lógico - individuos compuestos](paradigma-logico---individuos-compuestos.html)

### más características

Polimorfismo en el paradigma lógico Meter acá lo del polimorfismo en lógico, puede ser con el ejemplo del max.

### Predicados de Orden Superior

-   [Orden Superior](orden-superior.html)
-   [Paradigma Lógico - el forall](paradigma-logico---el-forall.html)

### Otros para ordenar

-   [Paradigma Lógico - cómo pienso la resolución de un punto](paradigma-logico---como-pienso-la-resolucion-de-un-punto.html)
-   [Polimorfismo en el Paradigma Lógico](polimorfismo-en-el-paradigma-logico.html)

