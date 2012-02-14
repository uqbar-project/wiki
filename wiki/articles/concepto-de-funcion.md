Concepto de Función
===================

Como su nombre lo indica, la función es el concepto fundamental del paradigma funcional: mientras que, por ejemplo, en el paradigma de objetos, los problemas se resuelven en términos de envío de mensajes entre objetos, en funcional, nuestros programas, se estructurarán en torno a aplicación de funciones sobre valores. La función, es así, pues, la [computación](computacion.html) característica del paradigma.

Enfoques
--------

`Si bien el concepto de función parece trivial, existen muchas áreas del saber en las que se utiliza este concepto. En general, en todas preserva un significado similar: una unidad con una salida y al menos una entrada, capaz de producir un resultado. Sin embargo`

### Función como transformación matemática

### Función como caja negra

### Función en teoría de categorías

### Función desde el cálculo lambda

#### Función como valor

#### Valores como funciones

### Función como un TAD

Las funciones, en tanto valores, pueden ser también tratadas

### Funciones en Haskell

`Haskell es un lenguaje funcional`

Programa como combinación de funciones
--------------------------------------

c. Concepto de función en \*Haskell\*. Toda función es un valor, pero todo valor, es una función? Las alternativas son:

-   Sí. Todo es una función. De allí la evaluación diferida. Aun sí

tiene cero argumentos.

-   No. Función es solo aquello que tiene tipo (-&gt;) a b. La

evaluación diferida es propia de las expresiones, sean o no funciones. Función es aquello que es aplicable, es decir, que representa una transformación con dominio e imagen.

En ambos casos, valdría la pena revisar la consistencia de estas definiciones con el cálculo lambda. Y también, si damos a Haskell por implementación pura o no del mismo.

La función es un tad

`  1. Me parece que mucho más interesante dividir al universo de funcional en valores y funciones (+1 a lo que dice Franco)`
`  2. Las funciones que no reciben parámetros ... ehhh quiero decir .... no existe tal cosa, una función es algo sensible de ser aplicado con parámetros`
`  3. Aquello que no es función es valor (no se si usaría el término constante, porque los valores en funcional son constantes no pueden cambiar)`
`  4. Aquello que puedo usar como argumento de una función o como valor de retorno de una función es un valor`
`  5. Existen funciones que pueden recibir funciones por parámetro o retornar una función`
`  6. Las funciones son valores`
