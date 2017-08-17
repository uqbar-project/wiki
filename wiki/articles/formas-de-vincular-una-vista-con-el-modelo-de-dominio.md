---
layout: article
title: Formas de vincular una vista con el modelo de dominio
featured: true
---

# Interacciones entre vista y modelo

Dado que el objetivo de la interfaz de usuario es permitir la interacción con el modelo de dominio, cada uno de los elementos que conforman la interfaz de usuario tendrá como tarea alguna parte de esta interacción, ya sea mostrar al usuario una porción del modelo de dominio, o bien permitirle realizar acciones que lo modifiquen.

Una forma simple de ordenar una interfaz de usuario es considerar que cada vista (que puede ser una ventana, una página o bien una porción bien delimitada de una vista más grande) tiene como responsabilidad interactuar con un único objeto de dominio. A este objeto de dominio lo denominaremos modelo de la vista.

Desde esa premisa básica podemos imaginar que cada vez que haya un control editable en una vista, ese control estará editando algún atributo del modelo. Luego, las modificaciones que realicemos sobre estos controles editables, de alguna manera deberán ser *impactadas* sobre los atributos asociados. Al componente de software que toma la responsabilidad de impactar esos cambios lo denominamos controller.

Los mecanismos para trasladar o impactar una modificación hecha por el usuario sobre cualquier control editable se pueden dividir en dos grandes grupos:

-   Vinculación indirecta o manual
-   Vinculación directa o automática (*binding*).

<!-- -->

# Vinculación indirecta

En este tipo de estrategias los controles editables de la UI toman la responsabilidad de almacenar los valores que va ingresando el usuario a la espera de un evento que *dispare* la ejecución de la operación (en aplicaciones web este evento suele ser el *submit* de un formulario HTTP).

Naturalmente, ante esta disociación, el objeto de dominio asociado no se ve afectado por las acciones del usuario hasta que no se produzca el evento antes mencionado. En el momento que se produce el *submit*, el controller deberá tomar la responsabilidad de leer todos los valores del formulario de los controles en los que fueron almacenados y recién ahí impactarlos en el modelo de dominio.

De esta forma, cualquier validación o modificación dinámica de los valores en la vista deberá ser realizada sin la intervención del dominio o bien esperar hasta el momento del submit para poder llevarse a cabo. Esto incluye a muchos de los comportamientos denominados *ricos* de una UI, por ejemplo:

-   Validaciones realizadas a medida que se cargan los datos en un formulario
-   Valores que dependen unos de otros (un caso típico: combos anidados).
-   Controles que se habilitan o deshabilitan dependiendo de los demás valores ingresados.
-   ... entre otros.

La principal desventaja de este esquema radica en la no-intervención del modelo de dominio durante la interacción con el usuario, lo que impide aprovechar toda la lógica que contenga este objeto, que deberá tomar uno de dos caminos:

-   Esperar al momento de submit para intervenir, con lo cual se pierden muchas de las posibilidades de UI *rica* enunciadas en la enumeración anterior.
-   Duplicar la lógica ya contenida en el modelo de dominio, replicándola en la interfaz de usuario de forma de poder realizar validaciones y otros comportamientos dinámicos basándose en el contenido de los controles.

La primera de las opciones produce una limitación en las posibilidades que la UI le puede brindar al usuario, haciéndolo en muchos casos inviable o en otros perdiendo calidad en la UI. La segunda produce una duplicación de la lógica que podemos analizar desde dos perspectivas distintas.

Por un lado, toda duplicación de código genera una potencial inconsistencia ante la posibilidad de que una de las dos partes se modifique quedando la otra desactualizada.

Por el otro, la lógica propia del dominio metida dentro de la interfaz de usuario resulta mucho más incómoda de programar ya que deberemos adaptarnos a las restricciones tecnológicas de componentes que están diseñados prioritariamente para la interacción con el usuario. Como un ejemplo de las dificultades que aparecen, podemos mencionar que en lugar de tener la información necesaria almacenada en variables que tengan un tipo y toda la potencia de nuestro lenguaje de elección, deberemos tener los valores contenidos en controles, en muchos casos sin poder exigir que sean de un tipo determinado. Y entonces lo que para nosotros en el dominio es un número o una fecha en la UI será un String o lo que para nosotros es una referencia a otro objeto de dominio en la UI será un código (String) o un índice numérico en una lista.

Naturalmente todas estas restricciones hacen al código más complejo y más propenso a errores, lo que nos lleva a evaluar la siguiente alternativa.

<!-- -->

# Binding

En este esquema, lo que se buscará es automatizar el pasaje de información entre la vista y el dominio. Es decir, se proveerá una descripción ([declarativa](declaratividad.html)) de la vinculación entre los componentes visuales y el modelo de dominio para que un componente genérico se ocupe de mantenerlos mutuamente sincronizados.

A esta descripción de la vinculación entre ambos la solemos llamar binding y puede incluir entre otras cosas:

-   Un mapeo entre un componente visual y un elemento del dominio, típicamente cada control de la vista estará asociado a un atributo de un objeto de domino.
-   Conversiones a realizar (por ejemplo si el valor a ingresar es una fecha y se ingresa desde la UI como texto, deberá proveerse el formato esperado y la lógica para convertir de ese formato a la representación interna de fechas que use el modelo de dominio).
-   Validaciones a realizar (dado que uno de los objetivos es aprovechar la lógica del modelo de dominio, normalmente las validaciones -salvo las propias de la conversión- serán delegadas en el modelo de dominio y por lo tanto la descripción de la validación consistirá en algún mecanismo para indicar qué consulta realizar sobre el dominio para poder efectuar la validación en cuestión).

Esta estrategia busca fundamentalmente dos objetivos:

-   Simplificar la sincronización entre ambas partes de la aplicación, basándose en componentes reutilizables.
-   Aprovechar la lógica contenida en el modelo del dominio para tomar acciones durante su propia edición.

El segundo de estos objetivos es el que suele proponer a veces algunas dificultades, ya que para aprovechar la lógica se necesita impactar las modificaciones realizadas sobre la UI directamente sobre el objeto de dominio. En los casos de aplicaciones que tienen un comportamiento transaccional desde el punto de vista del usuario, esta acción directa sobre el dominio implica algún mecanismo para garantizar que en caso de cancelar la operación el objeto queda sin cambios, en su estado original antes de comenzar.

Adicionalmente esta fuerte vinculación entre la vista y el dominio nos puede presentar dificultades si la vista tiene requerimientos que no son fácilmente atribuibles a un objeto del dominio, es decir, comportamiento específico de la vista. Ejemplos de comportamiento propios de la vista podrían ser paginar una grilla o dividir la información del objeto entre múltiples viñetas o *tabs*.

Se necesitan entonces herramientas para manejar el nivel de acoplamiento entre la vista y el modelo de dominio, tanto por cuestiones de transaccionalidad como para poder asociar comportamiento no dependiente del dominio.

<!-- -->

## Binding transaccional

Citamos a continuación algunos de los mecanismos utilizados para desvincular el dominio de la vista para proveer a nuestra aplicación de un comportamiento transaccional:

### Aprovechamiento de la transaccionalidad de la persistencia

Es frecuente encontrar aplicaciones donde la transaccionalidad está delegada en el mecanismo persistente, con frecuencia una base de datos relacional. Si el modelo de dominio de la aplicación es persistido en un mecanismo con soporte transaccional y además el ciclo de vida de los componentes de dominio está dominado por la persistencia (es decir, el objeto de dominio dura en memoria sólo durante una transacción y luego es descartado), entonces simplemente cancelando la transacción de persistencia se logra descartar los cambios realizados al modelo de dominio. Esta estrategia es muchas veces la más simple, y si bien tiene algunas limitaciones técnicas, es una de las más utilizadas.

###  Postergación del binding mediante copias o wrappers

Esta estrategia se basa en bindear la vista contra un objeto que no sea exactamente el dominio, para luego volcar los datos sobre el dominio en un paso posterior (probablemente en el submit). Esto permite garantizar que el dominio no se verá modificado hasta finalizar la acción del usuario, al mismo tiempo que trabajar el comportamiento dinámico de la vista sobre un objeto independiente de la tecnología de presentación. De esta manera, esta estrategia presenta una alternativa intermedia entre una vinculación manual y una automática.

Existen dos variantes a esta estrategia. La primera de ellas es utilizar una copia del objeto de dominio a editarse, que luego deberá reemplazar al original en el dominio o bien trasvasar la información de un objeto a otro. La ventaja de esta estrategia es que permite reutilizar toda la lógica propia del objeto de dominio. El problema es que cuando se editan varios objetos relacionados desde la misma vista, impactar luego los cambios en el dominio puede resultar complejo.

La segunda variante es utilizar un wrapper u otro objeto que no sea de la misma clase que el original. Lo interesante de esta técnica es que provee un mayor desacoplamiento entre vista y modelo (ver *modelo de aplicación* en el apartado siguiente). La desventaja es que obliga a duplicar la información del objeto original en este nuevo objeto (Al menos en un lenguaje basado en clases, en lenguajes con mecanismos de herencia más flexibles esta problemática puede ser resuelta de otras maneras, como mixins o traits. Lamentablemente, la mayoría de los lenguajes más populares hoy en día no proveen este tipo de mecanismos.)

### Transaccionalidad a nivel de dominio  

Otra posibilidad es permitir que los objetos de dominio manejen la transaccionalidad, es decir, permitir que sean modificados y, en caso de ser necesario, delegar en ellos mismos la responsabilidad de volver atrás los cambios cancelados por el usuario. Esto puede ser hecho manualmente (aunque puede resultar engorroso) o de forma automática, por ejemplo mediante aspectos.


# Comportamiento a nivel de vista

A veces es necesario tener comportamiento en la vista que no es atribuible a ningún objeto de dominio. En ese caso algunas de las estrategias posibles son:

## Modelo de aplicación  

Llamamos modelo de aplicación a un objeto que tiene lógica que no es atribuible a un objeto de dominio, sin embargo es independiente de la tecnología, por lo tanto podemos considerarlo modelo. Un objeto de estas características nos provee de un espacio en donde colocar lógica utilizando las mismas herramientas del dominio pero sin tener que restringirnos a las limitaciones que solemos establecer sobre los objetos de domino.

Típicamente los casos de uso complejos de una aplicación tendrán un modelo de este tipo que contemple la lógica necesaria para llevarlos a cabo. Pueden tener tanto lógica de navegación como de visualización, aunque en algunos casos también se decide separar ambos tipos de lógica.

## Value Models  

Otra forma de desacoplar la vista y el modelo es proveyendo un almacenamiento intermedio para cada control, que guarda el valor manejado por el control hasta el momento del submit, en el cual será volcado al modelo de dominio. Al objeto que contiene el valor durante ese tiempo se lo denomina ValueModel y la principal diferencia con la estrategia anterior es que en ese caso se tenía un único intermediario para toda la vista, mientras que ahora tenemos un intermediario por cada control.

Un ValueModel provee de la posibilidad de definir trasnformaciones y validaciones a cada control. Una ventaja importante de este mecanismo es que provee de una forma sencilla de reutilizar la lógica de transformación y validación.

Por otro lado, la atomización de estos objetos dificulta la posibilidad de establecer lógica sobre este modelo que dependa de más de uno de los controles de la vista.

En los casos en que la relación entre la vista y el modelo de dominio es muy lejana, una solución posible es descartar el binding y pasar a una estrategia de interacción manual entre vista y dominio, aunque, claro, eso implica perder parte de las ventajas de automatizar este comportamiento.

<!-- -->

# Links relacionados

-   [Ejemplos de Binding entre vista y modelo](ejemplos-de-binding-entre-vista-y-modelo.html)
-   [Temario Algoritmos III](algo3-temario.html)

