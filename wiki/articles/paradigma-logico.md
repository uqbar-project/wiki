### Introducción

(Ver apunte) Capítulo 1 – De qué se trata esto de “paradigma lógico” 2 ¿Por qué lo de “lógico”? 2 Nuestro primer programa lógico – lenguaje Prolog 2 Entonces, ¿qué es un programa lógico? 2 Condiciones: “y” vs “o” 2

### Algunas características relevantes

El paradigma lógico trabaja con el principio de [Universo Cerrado](universo-cerrado.html) Hipótesis de universo cerrado 3 Consultas existenciales e inversibilidad de predicados 3 Negación 3 Variables ligadas y no ligadas – problemas con la inversibilidad 3 Generadores 3 Consultas existenciales e inversibilidad de predicados Contar que • una consulta puede tener múltiples respuestas • la inversibilidad se puede dar para predicados monádicos también • la inversibilidad de un predicado se puede analizar argumento x argumento, decimos que es “inversible” a secas cuando es inversible para todos sus argumentos. • No todos los predicados son inversibles, veremos casos de predicados que no resultan inversibles, y también una forma de solucionarlos para que los predicados sí queden inversibles. Negación Variables ligadas y no ligadas – problemas con la inversibilidad Generadores Contar cómo arreglan el problema de la inversibilidad

-   [Paradigma Lógico - un poco de nomenclatura](paradigma-logico---un-poco-de-nomenclatura.html)

### Manejo de conjuntos

forall – “a todos los que les pasa A, les pasa B” • Tener claro si uno quiere poner “uno” o “todos”, si es “todos” va forall, sino va simplemente la condición. Relacionar con cómo se ligan las variables. • Acá también hay problemas con la inversibilidad si ciertas variables no llegan ligadas. Solución con generadores. listas – conjuntos explícitos • Definición de lista: lista vacía o (cabeza y cola) • Pattern matching • Definiciones recursivas • findall o usarlo sólo si lo necesito todo esto con ejemplos de uso

### Tipos de datos en Prolog

Para qué sirven (los 2 casos) Pattern matching con functores – cómo acortan los programas

### Manejo de la Información

Meter acá lo de cómo hacer jugar el estado en el dominó. Sería ideal contar un poco de cómo representar info, p.ej. no ser “listeros”, después si necesito “a todos juntos” tengo el findall. También se puede meter un ejemplo en el que sí convenga modelar con listas, porque los elementos deben estar ordenados. P.ej. el del subte.

-   [Paradigma Lógico - individuos compuestos](paradigma-logico---individuos-compuestos.html)

### más características

Polimorfismo en el paradigma lógico Meter acá lo del polimorfismo en lógico, puede ser con el ejemplo del max.

### Inversibilidad

La [inversibilidad](paradigma-logico---inversibilidad.html) es la característica que blah, pero [no siempre](paradigma-logico---casos-de-no-inversibilidad.html) funca y en esos casos lo resolvemos con [generación](paradigma-logico---generacion.html)

### Predicados de Orden Superior

-   [Orden Superior](orden-superior.html)
-   [Paradigma Lógico - el forall](paradigma-logico---el-forall.html)

### Otros para ordenar

-   [Paradigma Lógico - cómo pienso la resolución de un punto](paradigma-logico---como-pienso-la-resolucion-de-un-punto.html)
-   [Polimorfismo en el Paradigma Lógico](polimorfismo-en-el-paradigma-logico.html)

