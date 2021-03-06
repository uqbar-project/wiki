---
layout: article
title: Diseno y tecnologia
---

Existe una fuerte relación entre diseño y la programación, o mejor dicho, entre el diseño y la tecnología que voy a utilizar para construir el sistema.

El nivel de detalle del diseño
------------------------------

En primer lugar podemos analizar el nivel de detalle de ambos. Obviamente el programa tendrá muchos detalles más que el diseño, necesita poder ser compilado y ejecutado, incluir todos esos detalles en la especificación de diseño sería contraproducente porque nos obligamos a decidir sobre cada uno de esos puntos y tal vez convenga postergar algunas decisiones para el momento de programar. En el extremo, si el diseño definiera todos los detalles del programa subyacente, entonces la programación posterior no requiere de tomar ninguna decisión y por lo tanto podría automatizarse. En ese caso la programación deja de tener sentido como actividad, la puedo considerar un paso más de la compilación y nuestro diseño podría considerarse directamente como un programa.

A medida que los lenguajes de programación fueron siendo cada vez de más alto nivel y fueron incorporando mejores abstracciones (podemos destacar el manejo automático de memoria como un ejemplo esencial), la diferencia entre el nivel de abstracción del diseño y del programa se fue achicando. En la literatura podemos encontrar dos visiones de este proceso. Algunas metodologías como MDA consideran que sólo debemos diseñar y debemos descartar la programación como actividad. Entonces la ingeniería de software debería dedicarse a producir modelos, y los programas deberían derivarse automáticamente de esos modelos. Otras metodologías como XP incorporan el diseño como parte de la actividad, y entonces consideran que la única actividad es la programación. Como referencia de este pensamiento puede leerse el artículo de Martin Fowler: [Is Design Dead?](http://martinfowler.com/articles/designDead.html)

Ambas visiones no son tan distintas entre sí, en definitiva el diseño y el programa se acercan, y finalmente la única discusión restante es si nuestros modelos/programas deberán tener forma de diagrama o si tendrán forma de texto. (¿Y si se combinan ambos? ¿Hay otras posibilidades?... todas son preguntas para las que hoy la ciencia del desarrollo de software no tiene respuestas únicas)

Por otro lado, un diseño de muy alto nivel deja muchos detalles a responsabilidad de la persona que va a construir. Si el modelo deja abiertas cuestiones que tienen que ver (por ejemplo) con la forma en que se estructurará el programa entonces está postergando decisiones que son de diseño aunque las tome una persona cuyo rol formal es el de programador. En ese caso lo que pasa es que el programador es el que está llevando a cabo el nivel más detallado del diseño. Esto no tiene por qué ser algo negativo, es una forma de organización perfectamente válida, en la cual algunas personas toman la responsabilidad del diseño a alto nivel (podría llamarse incluso arquitectura) y a otras les toca ocuparse del diseño más detallado.

Metamodelos en el diseño y metamodelos en el programa
-----------------------------------------------------

La otra perspectiva desde la que podríamos analizar la relación entre diseño y tecnología es pensar la relación entre el [metamodelo](metamodelo.html) que usamos para diseñar y el metamodelo del lenguaje que usaremos para construir.

Si esos metamodelos coinciden entonces lo que yo diseñe puede que sea más fácil de construir (o de visualizar la relación entre lo construido y el modelo). Pero, de nuevo, un metamodelo para un programa puede requerir de muchos detalles que en determinado momento del diseño todavía no estoy en condiciones de decidir. Un ejemplo común es tener que decidir si queremos poner una clase abstracta o una interfaz cuando todavía no hemos definido los detalles del concepto asociado.

Entonces dependiendo del nivel de detalle de nuestro diseño tal vez convenga elegir metamodelos más o menos parecidos a los metamodelos de nuestra tecnología subyacente:

-   Utilizar metamodelos sutilmente diferentes nos permite diseñar con más libertad, desprendiéndonos de complejidades y restricciones tecnológicas.
-   Pero las diferencias entre ambos metamodelos plantean "gaps" que luego habrá que resolver al construir, entonces no podemos permitir que ambos metamodelos tengan diferencias demasiado fundamentales.

En resumen, el diseño es la especificación de lo que quiero construir. Entonces, al elegir la forma en la que quiero diseñar, las herramientas que uso para diseñar tengo que entender la relación que establezco con la tecnología usada para la construcción.

-   Si las dos ideas se parecen mucho eso puede plantear restricciones muy tempranamente, si planteo actividades distintas es porque quiero ponerles focos distintos, usar herramientas distintas.
-   Pero si las dos ideas van por caminos muy distintos entonces puede que el diseño pierda utilidad, porque para poder construir voy a tener que llenar los huecos (gaps) que el modelo de diseño no cubra. En definitiva, no es posible construir sin llenar esos huecos, y llenar esos huecos es diseñar.

