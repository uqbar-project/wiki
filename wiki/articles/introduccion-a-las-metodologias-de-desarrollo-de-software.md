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

Metodologías predictivas / adaptativas
--------------------------------------

Ejemplos concretos
------------------

-   **Modelo de Cascada**: originado en el [paper de Winston Royce: "Managing the development of large software systems"](http://www.cs.umd.edu/class/spring2003/cmsc838p/Process/waterfall.pdf) ordena por fases el desarrollo de software. Si bien las fases pueden llegar a solaparse, el proceso está concebido como secuencial ("el diseño debe ocurrir antes que el desarrollo: STEP 1 - PROGRAM DESIGN COMES FIRST"), orientado al proceso ("Begin the design process with program designers, not analysts or programmers"), orientado a la documentación ("how much documentation? My own view is 'quite a lot'; certainly more than most programmers, analysts, or program designers are willing to do if left to their own devices", "If the documentation is in serious default my first recommendation is simple: replace project management. \[...\] Stop all activities not related to documentation") y definitivamente predictivo.
-   **Espiral de Boehm**: originado en el [paper de Barry Boehm: "A Spiral Model of Software Development and Enhancement"](http://www.cs.umd.edu/class/spring2003/cmsc838p/Process/spiral.pdf), surge como primer contracara del modelo de cascada. Sigue un modelo de planificación de objetivos, identificación de riesgos y desarrollo y verificación del producto en n iteraciones, lo que lo convierte en adaptativo (contempla cambios más allá del análisis previo). Las primeras iteraciones pueden consistir en prototipos. No dice nada de Orientado a proceso / orientado a doc.
-   **Proceso unificado**: comercializado por varias empresas, el [proceso unificado](http://en.wikipedia.org/wiki/Unified_Process) es el primero que permite adaptarse según el tamaño del proyecto y su complejidad. Se autodefine como un proceso "iterativo e incremental, dirigido por los casos de uso, orientado a la arquitectura y que busca anticipar los riesgos del proyecto". Las fases del proyecto se dividen en: Inception, Elaboration, Construction y Transition. Cada una es susceptible de dividirse en iteraciones y lo que diferencia una fase de otra es la madurez del producto que se está construyendo. Desde la fase de Elaboración hay construcción, test y deploy de un ejecutable, por eso su naturaleza es *iterativa*. De la misma manera, se evita tomar decisiones en forma prematura y se contempla la posibilidad de modificar los requerimientos hasta la última iteración, por lo que también encaja perfectamente en las metodologías adaptativas. UP sugiere una gran cantidad de artefactos de documentación, aunque podemos minimizarlos para concentrarnos en el código funcionando para el usuario. No obstante tomamos una frase que acerca un sesgo preferencial a la discusión filosófica documentación vs. producto: "El producto no es sólo el ejecutable, también es la documentación que acompaña al sistema: manual de usuario, casos de uso, diagramas de clase, diagrama de arquitectura, etc." Y por último destacamos como mérito otra frase extractada de los creadores: "Las personas son importantes y tienen que saber qué hacer", aunque luego define roles muy específicos que muestran que esas personas están supeditadas a un proceso en el cual confiar.
-   **Metodologías ágiles**: algunas implementaciones conocidas son Scrum, XP, ASD.

Links relacionados
------------------

-   [Volver a Diseño de Sistemas](design-temario.html)

