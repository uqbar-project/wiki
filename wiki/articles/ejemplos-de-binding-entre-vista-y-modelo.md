Control Text box
----------------

Queremos observar el valor de ese textbox para bindearlo contra un atributo del modelo. Algunos ejemplos, bindeando contra la propiedad value del textbox:

1.  nombre del cliente (variable de instancia nombre de la clase cliente)
2.  fecha de nacimiento de un cliente (variable fechaNacimiento de la clase cliente)
3.  la nota de un examen (variable nota de la clase examen)

Nosotros queremos que:

-   cuando el usuario cargue el nombre del cliente en la pantalla se actualice el modelo automáticamente
-   cuando alguien actualiza el modelo se dispare la notificación a la pantalla para que "refresque" el valor del control.

En el primer caso el binding se da naturalmente; en los otros dependemos de que la tecnología de UI tenga un control textbox cuyo value admita un Date/Number/etc. De otro modo vamos a necesitar un **conversor**, que permite transformar el valor recibido desde la interfaz de usuario (generalmente un String) al tipo requerido por el modelo, y viceversa.

-   Dos observers, que están asociados a un característica específica del modelo o del control. En el caso del modelo serán sus atributos, en el caso del control hasta ahora trabajamos con su valor, pero más adelante veremos que se pueden observar otras propiedades de un control.

En el caso de JFace, estos observers tienen la capacidad tanto de actualizar la propiedad correspondiente como de obtener su valor. En otras tecnologías podrían tener una sola de las dos capacidades o bien estar repartidas en objetos distintos.

-   Un contexto de binding, objeto responsable de recordar las diferentes parejas de modelo-control que están vinculados entre sí y de repartir los eventos teniendo en cuenta todas las complejidades que eso implica (evitar los ciclos, manejar la concurrencia, etc).

Esta responsabilidad bien podría ser de los observers, pero dada la complejidad del manejo de eventos en UI es común que esté separado en un objeto que se ocupe exclusivamente de esto.

Casos sencillos de conversiones pueden ser de String a Date o a un valor numérico. Más interesante es la conversión que realiza un combo, que asocia los elementos visuales del combo con los objetos que representan esas opciones, y puede pasar de uno a otro en ambas direcciones.

-   Validadores, que proveen diferentes puntos de chequeo del valor recibido.

En principio se deben contemplar al menos dos puntos de validación:

-   Antes de la conversión
-   Después de la conversión.

A estas validaciones pueden sumarse otras realizadas por el modelo de dominio, por ejemplo en el momento de setearle el valor.

Todos estos objetos en conjunto intervienen cada vez que se produce un evento en uno de los dos extremos de la cadena de binding, para hacer llegar el evento (y probablemente el valor asociado) hasta el otro extremo.

Otros usos del binding
----------------------

Vimos dos casos en los que usamos el binding contra propiedades del control que no sean su valor seleccionado:

-   El label que muestra los errores se hace invisible si no hay errores
-   El botón de aceptar se inhabilita si hay errores.

Mirando la clase SWTObservables podemos ver todas las propiedades que el framework en cuestión nos permite vincular, se puede hacer que un control no se pueda modificar, cambiar su color, etc.

También vimos que usando la interfaz ComputedValue podemos vincular cualquier expresión que deseemos.

Otros links relacionados
------------------------

[Binding: Vinculación entre la vista y el modelo](binding--vinculacion-entre-la-vista-y-el-modelo.html) [Algo3 Temario](algo3-temario.html)
