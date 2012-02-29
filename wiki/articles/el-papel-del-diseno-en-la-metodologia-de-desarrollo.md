El diseño cumple un rol fundamental en todos los desarrollos de software. Es entonces cuando tenemos que tomar decisiones que afectan directamente al producto que vamos a construir. Por supuesto, el qué del análisis vs. el cómo del diseño es una diferencia que aparece en todos los libros. Pero mientras en el qué del análisis las recomendaciones generales apuntan a ordenar, clasificar, separar

-   en requerimientos funcionales
-   y no funcionales

o bien

-   en discriminar requerimientos imprescindibles para la puesta en producción
-   de los importantes
-   o deseables

en el diseño necesitamos echar mano a herramientas mucho más cercanas a la tecnología.

### Qué es diseñar

Diseñar es encontrar:

-   los componentes de un sistema
-   la responsabilidad de cada componente (que justifica su existencia)
-   y la relación que cada componente tiene con los demás (sea temporal o más permanente en el tiempo)

Para eso tenemos que tomar decisiones, y aquí es donde empezamos a depender de la tecnología, porque ella nos define:

-   sus abstracciones
    -   ¿cómo modelo cada componente o entidad? ¿tengo tipos de dato propios?
    -   ¿cómo defino comportamiento? ¿puedo abstraer ese comportamiento?
    -   ¿cómo relaciono las entidades? ¿debo distinguir el medio en donde esas entidades están relacionadas?
-   sus limitantes
-   sus beneficios

### El diseño y la metodología

Si la metodología de trabajo es secuencial, cada fase del proyecto tiene un comienzo y un fin específico. El diseño debe respetar el orden que le corresponde: después del análisis y antes de la programación / construcción. Si la metodología de trabajo es iterativa, esto implica diseñar en diferentes momentos, sin acotarlo a un período determinado.

De la misma manera,

-   cuando la metodología es orientada al proceso, se genera una gran cantidad de documentación respaldatoria del diseño. El proceso es el eje central del proyecto, es el que guía a las personas a cada objetivo que quiere alcanzar (en el diseño esto se concreta con un documento entregable que es la especificación). Si el proceso está bien definido sólo hay que controlar el avance de las tareas.

<!-- -->

-   cuando la metodología es orientada al producto final, nos interesa más dejar en claro las decisiones importantes que respalden el software que está corriendo. La interacción entre las personas es fundamental y termina definiendo el proceso de diseño, de la misma manera que el éxito o el fracaso del mismo.

### Diseño anticipado y diseño iterativo

Cada metodología nos lleva a dos formas de diseñar completamente diferentes:

El **diseño anticipado** se asume que

-   el análisis ya ha relevado todos los procesos que el usuario necesita
-   tenemos disponible toda la información para poder definir cada proceso
-   no habrá cambios en los requerimientos hasta nuestra implementación
    -   el usuario no hará cambios o nuevos pedidos.
    -   no habrá cambios externos al sistema que obliguen a modificarlo (como disposiciones legales).
-   si el diseño es adecuado, la codificación se ajustará perfectamente a lo que el usuario necesita.
    -   para ello hay que documentar el sistema en su completitud para que los programadores no tengan que tomar decisiones de diseño en la codificación.
-   en la fase de diseño no se debe programar, dado que se estaría solapando la actividad (de la misma manera que en la fase de codificación no se debe diseñar)

Por el contrario, el **diseño iterativo** asume que

-   sólo tenemos algunos procesos relevados, y aunque los tuviéramos en su totalidad, los requerimientos podrían cambiar.
-   es inocente pensar en que no habrá cambios en los requerimientos, dado que
    -   el usuario no sabe exactamente lo que se va a construir y tiene derecho a pedir modificaciones cuando se da cuenta de que cometió un error al dar información al diseñador.
    -   bajo la premisa anterior el diseñador no puede realizar un diseño que no esté sujeto a cambios, por los errores propios que además podría cometer.
-   si el diseño no es adecuado, debemos cambiarlo lo más pronto posible. Esto incluye la fase de codificación.
-   si queremos reflejar la realidad, tenemos que permitir que haya alternancia entre diseño y programación. No paralelizamos las actividades, sino que una se va solapando a la otra, como en una pila.
-   el diseño iterativo considera que los errores son parte del desarrollo mismo y necesitamos poder modificar el diseño en cualquier momento, sin que eso paralice el proyecto (iterativo tiene mucho de "prueba y error").

### Análisis comparativo

Los defensores del diseño anticipado sostienen que en el diseño iterativo se pierde el orden, que es difícil de coordinar un proyecto (no se sabe exactamente en qué porcentaje está cumplida cada actividad), y que el diseño iterativo confunde diseño y programación, al punto en el que en realidad sólo se programa.

Los defensores del diseño iterativo creen en que todos los proyectos se construyen de esta manera, lo único que hacemos al utilizar esta metodología es reflejar lo que sucede en la realidad: los requerimientos cambian (aparecen nuevos, se modifican los existentes y algunos incluso desaparecen durante el proyecto), los diseñadores se equivocan, también lo hace el usuario y continuamente nos vemos obligados a adaptar nuestra planificación. Separar un proyecto en varias iteraciones facilita aceptar esos cambios, porque no se mantienen fijos los requerimientos ni los diseños, solamente mantenemos el plazo de entrega (lo que vamos a entregar está sujeto a cambios en cada iteración).

### Integración de las actividades de diseño en el proceso de desarrollo

¿Qué actividades ocurren al diseñar?

-   Interactuamos con el usuario para repreguntar o proponer alternativas a lo que él solicitó
-   Interactuamos con el equipo de desarrollo, en donde pueden surgir
    -   inconsistencias en las definiciones que no habíamos detectado antes: en el diseño iterativo este "inconveniente" está previsto, no así en el diseño anticipado en donde el equipo del proyecto debe asumir el costo de este imprevisto.
    -   dificultades técnicas de implementación: esto suele ser más frecuente de lo que imaginamos, ya sea porque subestimamos la dificultad de un requerimiento, porque no tuvimos en cuenta algún factor tecnológico o porque los imprevistos suceden en todo proyecto.

Mientras que las metodologías secuenciales ven que el análisis condiciona el diseño y éste a su vez define las decisiones de implementación, es interesante notar que en los casos que mencionamos arriba es al revés: la arquitectura (o más general, las cuestiones técnicas) impactan sobre el diseño y el diseño puede hacer variar lo relevado en el análisis. De hecho, este es el valor agregado de un buen diseñador: hacer las preguntas que disparen mejoras en lo que el usuario pide.

Links relacionados
------------------

-   [Volver a Diseño de Sistemas](design-temario.html)

