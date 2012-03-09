¿Para qué sirve una metodología?
--------------------------------

Una metodología nos ordena, nos contiene, nos permite definir límites. Construir software complejo requiere un gran esfuerzo: tecnología, dinero y sobre todo: personas. Personas que interactúan entre sí, con diferentes grados de conocimiento, con diferentes roles, con diferentes intereses. Una metodología propone un esquema de trabajo que nos permite entender cuál es nuestro rol dentro del proyecto, nos acerca una cierta sensación de tranquilidad, de seguridad. Sin un proceso no sabemos cómo comenzar y cuándo terminar.

Metodologías secuenciales / iterativas
--------------------------------------

En las metodologías secuenciales, el proceso de desarrollo de software se divide en varios pasos o fases. Cada fase tiene un conjunto de metas a cumplir. El fin de cada fase delimita el comienzo de la fase siguiente. Aunque son normales la superposición de fases, estas metodologías proponen una gran fase de análisis de requerimientos, otra de diseño, otra de construcción y otra de pruebas donde el alcance de cada fase es la totalidad de los requerimientos de un proyecto.

En las metodologías iterativas se divide el proyecto en entregas o iteraciones. Si cada iteración define un conjunto de metas a cumplir, podríamos pensar que no hay una gran diferencia con la metodología secuencial. No obstante, cada iteración define como entregable un software testeable por el usuario. Entonces hay etapas de análisis de requerimientos, diseño, construcción y prueba en cada iteración. Además, cada iteración permite revisar y cambiar los requerimientos a resolverse.

Metodologías orientadas al proceso / a las personas
---------------------------------------------------

Otra taxonomía que divide las metodologías es el grado de importancia que le dan

-   al proceso de desarrollo
-   y a las personas que ejecutan ese plan

Si bien la mayoría de las metodologías contemplan tanto la serie de pasos que conforman el proceso como qué tipo de tareas deben desarrollar las personas (en base a sus perfiles), hay metodologías en las que el proceso está por encima de las personas. Dicho de otra manera, "respetar el proceso garantiza el éxito del proyecto". El margen de discrecionalidad (cuánto puedo salirme del libreto) es mínimo, sólo en lo operacional (en el día a día). Por eso es importante para estas metodologías poder medir cada tarea del proyecto, sea un ejecutable o documentación.

Por el contrario, las metodologías orientadas a las personas consideran que éstas definen el éxito o fracaso de un proyecto. Les asignan un grado mayor de decisión en cada tarea y confían en su capacidad de resolución de un problema antes que en las métricas que dan los indicadores. Esto no quiere decir que el proceso no importe, sino que ocupa un puesto de menor relevancia en la consecución de un logro.

Metodologías orientadas a la documentación / al producto
--------------------------------------------------------

Hay metodologías que sostienen que un producto de software bien elaborado nace de una documentación extensa y que contemple todas las decisiones que surgieron del análisis y del diseño. De esa manera el desarrollador no tendrá dudas ni excusas a la hora de escribir cada línea de código.

Por el contrario, hay metodologías que privilegian tener un software testeable para el usuario antes que tener el documento que respalde ese software que está corriendo. Esto no significa que haya que programar sin tener una especificación, sino que:

-   tomamos una documentación que puede tener definiciones pendientes, estar incompleta en su diseño o que sepamos que esté sujeta a cambios
-   construimos el software
-   y luego actualizamos las decisiones principales en el documento

con la ventaja de tener la certeza de que lo que hace el sistema es eso.

Metodologías predictivas / adaptativas
--------------------------------------

¿Qué ocurre con los cambios que piden los usuarios mientras se va construyendo el software? ¿Cómo se manejan?

-   algunas metodologías creen que es posible anticipar los cambios a través de un buen análisis y un buen diseño que contemple diferentes alternativas. Este enfoque no es inocente, sabe perfectamente que el usuario puede cambiar de opinión, que las disposiciones legales e impositivas sufren modificaciones y que los proyectos están siempre sujetos a vaivenes políticos. Pero justamente por eso busca minimizar los cambios para conservar lo más estable posible el entorno: resistirse al cambio es su naturaleza.
-   otras metodologías consideran que el cambio es inevitable, que no tiene sentido resistirse a él. De manera que el proceso mismo contempla momentos en los que el usuario puede modificar los requerimientos: esto implica agregar nuevos, descartar otros o modificarlos (no importa si ya fueron construidos o no). "El usuario tiene derecho a cambiar de opinión", sostienen.

Algunos ejemplos de metodologías
--------------------------------

-   **Modelo de Cascada**: originado en el [paper de Winston Royce: "Managing the development of large software systems"](http://www.cs.umd.edu/class/spring2003/cmsc838p/Process/waterfall.pdf) ordena el desarrollo de software en las conocidas fases de Relevamiento (de requerimientos de sistema y de software), Análisis, Diseño, Codificación, Prueba y Mantenimiento. Si bien las fases pueden llegar a solaparse, el proceso está concebido como
    -   secuencial ("el diseño debe ocurrir antes que el desarrollo: STEP 1 - PROGRAM DESIGN COMES FIRST"),
    -   orientado al proceso ("Begin the design process with program designers, not analysts or programmers"),
    -   orientado a la documentación ("how much documentation? My own view is 'quite a lot'; certainly more than most programmers, analysts, or program designers are willing to do if left to their own devices", "If the documentation is in serious default my first recommendation is simple: replace project management. \[...\] Stop all activities not related to documentation")
    -   y definitivamente predictivo.
-   **Espiral de Boehm**: originado en el [paper de Barry Boehm: "A Spiral Model of Software Development and Enhancement"](http://www.cs.umd.edu/class/spring2003/cmsc838p/Process/spiral.pdf), surge como primer contracara del modelo de cascada. Sigue un modelo de planificación de objetivos, identificación de riesgos y desarrollo y verificación del producto en *n iteraciones*,
    -   esto lo convierte en adaptativo (contempla cambios acomodándolos en el plan de las sucesivas iteraciones). Tomamos prestada una fase del artículo donde sostiene la importancia de que el proceso no fuerce al diseñador a tomar decisiones en forma anticipada: "\[The spiral approach\] fosters the development of specifications that are not necessarily uniform, exhaustive, or formal, in that they defer detailed elaboration of low-risk software elements and avoid unnecesary breakage in their design until the high-risk elements of the design are stabilized".
    -   El modelo de espiral tiene un contenido altamente orientado al proceso (en el artículo no hay una sola mención sobre el papel que juegan las personas dentro del proyecto)
    -   No podemos afirmar que sea una metodología orientada al producto ni a la documentación, en cada iteración debemos armar el análisis de riesgo, relevar los requerimientos de sistema y de software, hacer el plan de la iteración siguiente, el diseño global y detallado y los casos de prueba, pero por otra parte también la iteración contempla la posibilidad de prototipar y exige la presentación de un software testeable para el usuario.
-   **Proceso unificado**: comercializado por varias empresas, el [proceso unificado](http://en.wikipedia.org/wiki/Unified_Process) es el primero que permite adaptarse según el tamaño del proyecto y su complejidad. Se autodefine como un proceso "iterativo e incremental, dirigido por los casos de uso, orientado a la arquitectura y que busca anticipar los riesgos del proyecto". Las fases del proyecto se dividen en: Inception, Elaboration, Construction y Transition. Cada una es susceptible de dividirse en iteraciones y lo que diferencia una fase de otra es la madurez del producto que se está construyendo.
    -   Desde la fase de Elaboración hay construcción, test y deploy de un ejecutable, por eso su naturaleza es *iterativa*.
    -   De la misma manera, se evita tomar decisiones en forma prematura y se contempla la posibilidad de modificar los requerimientos hasta la última iteración, por lo que también encaja perfectamente en las metodologías adaptativas.
    -   UP sugiere una gran cantidad de artefactos de documentación, aunque podemos minimizarlos para concentrarnos en el código funcionando para el usuario. En la discusión filosófica documentación vs. producto podemos decir que UP está a mitad de camino de ambas, como lo confirma esta frase: "El producto no es sólo el ejecutable, también es la documentación que acompaña al sistema: manual de usuario, casos de uso, diagramas de clase, diagrama de arquitectura, etc."
    -   Respecto a la orientación persona o proceso podemos decir que UP se mantiene en una zona gris. Por un lado define que "las personas son importantes y tienen que saber qué hacer", mientras que al mismo tiempo cada persona cumple un rol que sugiere que esas personas están supeditadas a un proceso en el cual confiar.
-   **Metodologías ágiles**: como [extreme Programming](http://www.extremeprogramming.org/), Scrum (según el artículo de [Jeff Sutherland y Ken Schwaber](http://www.scrum.org/storage/scrumguides/Scrum_Guide.pdf), basado en el artículo original de [Takeuchi, Hirotaka; Nonaka, Ikujiro](http://hbr.org/product/new-new-product-development-game/an/86116-PDF-ENG)), ASD, etc. Todas se basan en el [manifiesto ágil](http://agilemanifesto.org/) que establece sus principios metodológicos para desarrollar software:
    -   Las personas y cómo se relacionan entre sí son más importantes que los procesos y las herramientas tecnológicas (es orientado a las personas, por definición)
    -   Un producto funcionando es más importante que la documentación exhaustiva (orientado al producto)
    -   La colaboración del cliente es más importante que la negociación del contrato (que discrimina al cliente como parte externa del producto)
    -   Responder al cambio es más importante que seguir el plan (adaptativo/iterativo)

Links relacionados
------------------

-   [Volver a Diseño de Sistemas](design-temario.html)
-   ["The New Methodology", artículo de Martin Fowler](http://martinfowler.com/articles/newMethodology.html)

