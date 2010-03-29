Dado que el objetivo de la interfaz de usuario es permitir la interacción con el modelo de dominio, cada uno de los elementos que conforman la interfaz de usuario tendrá como tarea alguna parte de esta interacción, ya sea mostrar al usuario una porción del modelo de dominio, o bien permitirle realizar acciones que lo modifiquen.

Una forma simple de ordenar una interfaz de usuario es considerar que cada vista (que puede ser una ventana, una página o bien una porción bien delimitada de una vista más grande) tiene como responsabilidad interactuar con un único objeto de dominio. A este objeto de dominio lo denonimaremos [modelo de la vista](modelo-de-la-vista.html).

Desde esa premisa básica podemos imaginar que cada vez que haya un control editable en una vista, ese control estará editando algún atributo del modelo. Luego, las modificaciones que realicemos sobre estos controles editables, de alguna manera deberán ser *impactadas* sobre los atributos asociados. Al componente de software que toma la responsabilidad de impactar esos cambios lo denominamos [controller](controller.html).

Los mecanismos para trasladar o impactar una modificación hecha por el usuario sobre cualquier control editable se pueden dividir en dos grandes grupos:

-   Vinculación indirecta o manual
-   Vinculación directa o automática (*binding*).

Vinculación indirecta
---------------------

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

Binding
-------

Comparación de ambas soluciones
-------------------------------
