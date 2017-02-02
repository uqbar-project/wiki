---
layout: article
title: Metamodelo
---

Un modelo es una descripción abstracta que describe aquello que se desea construir. Al definir un **modelo** uno sigue un conjunto de reglas. *Por ejemplo:* en un diagrama de secuencia las flechas (mensajes) entre las líneas deben darse de determinadas maneras, no pueden tener cualquier organización porque no tendría sentido. A este conjunto de reglas lo denominamos **metamodelo**.

El metamodelo nos dice:

-   qué tipo de abstracciones podemos usar en nuestro modelo (por ejemplo: clases)
-   qué características podemos asociarle a esas abstracciones (por ejemplo: métodos, atributos)
-   qué mecanismos tenemos para relacionar esas abstracciones entre sí (herencia, asociación)

Al construir, si utilizamos un lenguaje de alto nivel (o sea, no assembler) ese lenguaje también tendrá la capacidad de especificar abstracciones dentro del lenguaje, como procedimientos, funciones, tipos abstractos de datos, clases, objetos, herencia, polimorfismo, aspectos, mixins, servicios, componentes, etc. Obviamente, las abstracciones que puedo construir en un lenguaje dado, dependen también de un conjunto de reglas; dicho de otra manera el lenguaje tiene su propio metamodelo.
