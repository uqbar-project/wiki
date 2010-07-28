Introducción
------------

Recordemos nuestro objetivo:

-   cuando el usuario carga un dato en la pantalla queremos que se actualice el modelo automáticamente

pero también

-   cuando alguien actualiza el modelo (no necesariamente desde la UI) queremos que se dispare la notificación a la pantalla para que "refresque" el valor del control.

Esta relación se determina a través de dos observers, que están asociados a un característica específica del modelo o del control. En el caso del modelo serán sus atributos, en el caso del control puede ser su valor u otra propiedad. Para JFace, estos observers tienen la capacidad tanto de actualizar la propiedad correspondiente como de obtener su valor. En otras tecnologías podrían tener una sola de las dos capacidades o bien estar repartidas en objetos distintos.

Dónde manejar el binding bidireccional
--------------------------------------

-   Un contexto de binding, objeto responsable de recordar las diferentes parejas de modelo-control que están vinculados entre sí y de repartir los eventos teniendo en cuenta todas las complejidades que eso implica (evitar los ciclos, manejar la concurrencia, etc).

Esta responsabilidad bien podría ser de los observers, pero dada la complejidad del manejo de eventos en UI es común que esté separado en un objeto que se ocupe exclusivamente de esto.

TODO: Revisar el objeto que maneja el mapa de propiedades observadas en JFace.

Control Textbox
---------------

Dependiendo del framework el textbox puede llamarse text, input (type text), input field, etc. A partir de aquí utilizaremos en forma genérica al textbox para referirnos al elemento gráfico de forma rectangular que permite ingresar caracteres (por default, alfanuméricos).

Queremos observar el valor de ese textbox para bindearlo contra un atributo del modelo. Algunos ejemplos de binding contra la propiedad value del textbox:

1.  un *String*: nombre del cliente (variable de instancia nombre de la clase cliente)
2.  un *Date*: fecha de nacimiento de un cliente (variable fechaNacimiento de la clase cliente)
3.  un *Number*: la nota de un examen (variable nota de la clase examen)

En el primer caso el binding se da naturalmente; en los otros dependemos de que la tecnología de UI tenga un control textbox particular cuyo value sea de tipo Date/Number/etc. De otro modo vamos a necesitar

-   un **conversor**, que transforme el valor recibido desde la interfaz de usuario (generalmente un String) al tipo requerido por el modelo y viceversa.
-   **validadores**, que proveen diferentes puntos de chequeo del valor recibido. En principio se deben contemplar al menos dos puntos de validación: antes y después de la conversión.

### Otras propiedades para curiosear

Además del *value*, tenemos otras propiedades que pueden ser de interés:

-   read only (locked/enabled): un valor booleano que habilite/inhabilite el input desde el teclado
-   visible: el control aparece como visible/invisible en el formulario
-   width: el tamaño que tiene, que a veces está ligado a
-   size: la cantidad de caracteres que permite cargar

Control Checkbox
----------------

Dependiendo del framework se los denomina checkbox, input (type check), etc.

Es un control pensado para modelar atributos booleanos del modelo (a través de la propiedad value), de forma cuadrada con un tilde marcado o desmarcado según la propiedad sea true/false respectivamente. A los checkboxes se les puede setear por lo general los atributos read only y visible.

Control Combo
-------------

El combo necesita tener asociado:

-   una lista de elementos "seleccionables"
-   el elemento seleccionado que se puede manejar con un índice int, o bien es un objeto de esa lista. Ahora ¿de qué tipo es esa lista?

Si trabajamos con generics, podríamos pensar en un Combo<T> de manera que el modelo subyacente sea un Collection<T> y el elemento seleccionado un T. Otra alternativa menos elegante es trabajar con strings que mapeen identificadores o claves contra elementos seleccionados.

Independientemente de la manera en que trabajemos, vamos a necesitar tener un **conversor** para asociar los elementos visuales del combo con los objetos que representan esas opciones y pasar de uno a otro en ambas direcciones.

*Ejemplo:* si modelamos una aplicación para un Videoclub, cada película se relaciona con un género. Entonces la pantalla de carga de una película tiene:

-   un TextBox título
-   un TextBox año de filmación (numérico)
-   un ComboBox que selecciona un género

Cuando el usuario cambia la selección del combo

Revisar:

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
