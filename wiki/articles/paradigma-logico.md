¿Por qué “lógico”?
------------------

Porque está relacionado con conceptos de lógica matemática, en particular:

-   predicado
-   sintaxis
-   fórmula
-   razonamiento
-   deducción

Es un paradigma [declarativo](declaratividad.html) No hay [asignaciones destructivas](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html), se trabaja con el concepto de [Unificación](unificacion.html)

-   [Paradigma Lógico - un poco de nomenclatura](paradigma-logico---un-poco-de-nomenclatura.html)

En este apunte vamos a usar el lenguaje Prolog, que es (en una primera visión) una restricción del lenguaje de la lógica de predicados con una sintaxis distinta. Para dar una introducción intuitiva a la sintaxis de Prolog, traduzcamos el ejemplo de Sócrates. mortal(X):- hombre(X). hombre(socrates).

introducir el concepto de cláusula, y contar lo de cabeza y cuerpo. distinguir hechos de reglas. contar que en el cuerpo pueden ir varias condiciones, separadas por comas que representan un “y”. contar que se le pueden hacer consultas (quedarnos con las individuales) … Entonces, ¿qué es un programa lógico? Un programa lógico es un conjunto de definiciones de predicados. Cada definición puede tener varias cláusulas. Meter otro ejemplo y comentarlo un poco

### Conjuncion y disyunción

Condiciones: “y” vs “o” Esta sección es para que detecten cuándo es “y” y cuándo es “o”, separando en cláusulas cuando es “o” y poniendo todo en la misma cuando es “y”.

### Negación

Algunas Características Relevantes
----------------------------------

### Tipos de consulta

Existen dos tipos de consulta:

;Individuales:Se hacen sobre un individuo específico. Por ejemplo:<code>

    ?- mortal(socrates).

</code>

;Existenciales:Se busca algún individuo que satisfaga la relación. Por ejemplo:<code>

    ?- mortal(X).

</code> Nótese en el segundo tipo de consulta la presencia de [variables o incógnitas](variables-o-incognitas.html). Por ese motivo este tipo de consultas también son llamadas consultas variables.

El paradigma lógico trabaja con el principio de [Universo Cerrado](universo-cerrado.html).

### Inversibilidad

Esta característica de permitir realizar diferentes tipos de consulta sobre un predicado se denomina [inversibilidad](paradigma-logico---inversibilidad.html).

Sin embargo no todos los predicados admiten todas las formas de consulta la inversibilidad de un predicado se puede analizar argumento x argumento, decimos que es “inversible” a secas cuando es inversible para cada uno de sus argumentos.

Los motivos para que un predicado [no sea inversible](paradigma-logico---casos-de-no-inversibilidad.html) pueden ser, entre otros, por la utilización en su definición de:

-   comparación (/=, &lt;, &gt;, etc)
-   operaciones aritméticas
-   negación
-   predicados de orden superior,

Más específicamente la no-inversibilidad se da cuando en la definición de ese predicado se evalúa alguna de las expresiones arriba mencionadas utilizando variables sin ligar. Por eso la forma de solucionar este problema es utilizando predicados auxiliares para ligar las variables antes de llegar al punto problemático, a esta técnica se la denomina [Generación](generacion.html)

-   una consulta puede tener [múltiples respuestas](multiples-respuestas.html)
-   la [generación](paradigma-logico---generacion.html) se puede dar para predicados monádicos también

### Lógica de orden superior

-   [Orden Superior](orden-superior.html)

Manejo de conjuntos
-------------------

-   [Paradigma Lógico - individuos compuestos](paradigma-logico---individuos-compuestos.html)

### Listas

listas – conjuntos explícitos • Definición de lista: lista vacía o (cabeza y cola) • Pattern matching • Definiciones recursivas • findall o usarlo sólo si lo necesito todo esto con ejemplos de uso

### Forall

forall – “a todos los que les pasa A, les pasa B” • Tener claro si uno quiere poner “uno” o “todos”, si es “todos” va forall, sino va simplemente la condición. Relacionar con cómo se ligan las variables. • Acá también hay problemas con la inversibilidad si ciertas variables no llegan ligadas. Solución con generadores.

-   [Paradigma Lógico - el forall](paradigma-logico---el-forall.html)

### Estructuras de datos

Para qué sirven (los 2 casos) Pattern matching con functores – cómo acortan los programas

Manejo de la información
------------------------

Meter acá lo de cómo hacer jugar el estado en el dominó. Sería ideal contar un poco de cómo representar info, p.ej. no ser “listeros”, después si necesito “a todos juntos” tengo el findall. También se puede meter un ejemplo en el que sí convenga modelar con listas, porque los elementos deben estar ordenados. P.ej. el del subte.

-   [Paradigma Lógico - cómo pienso la resolución de un punto](paradigma-logico---como-pienso-la-resolucion-de-un-punto.html)

### Más características

-   [Polimorfismo en el Paradigma Lógico](polimorfismo-en-el-paradigma-logico.html)

