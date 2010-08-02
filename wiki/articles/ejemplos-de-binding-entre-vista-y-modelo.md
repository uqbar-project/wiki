Introducción
------------

Recordemos nuestro objetivo:

-   cuando el usuario carga un dato en la pantalla queremos que se actualice el modelo automáticamente

pero también

-   cuando alguien actualiza el modelo (no necesariamente desde la UI) queremos que se dispare la notificación a la pantalla para que "refresque" el valor del control.

Esta relación se determina a través de dos observers, que están asociados a un característica específica del modelo o del control. En el caso del modelo serán sus atributos, en el caso del control puede ser su valor u otra propiedad. Para JFace, estos observers tienen la capacidad tanto de actualizar la propiedad correspondiente como de obtener su valor. En otras tecnologías podrían tener una sola de las dos capacidades o bien estar repartidas en objetos distintos.

Dónde manejar el binding bidireccional
--------------------------------------

Para que esta lógica de notificación bidireccional entre modelo y vista funcione, recordemos la técnica que se usa con los observers, también llamados listeners:

-   en un primer momento se registran los interesados (addObserver)
-   cuando se produce un evento de interés para los interesados, se dispara la notificación (en método update del dominio, se envía mensaje notify a cada observer)

Volviendo al ejemplo modelo-vista,

-   al crear la UI debemos enlazar cada propiedad de un objeto de dominio con un control específico (addObserver, se registra el binding)
-   cuando la propiedad del objeto de dominio cambia, se dispara la notificación a todos los controles que observan esa propiedad (no necesariamente uno solo, podrían ser varios)
-   a su vez, cuando el usuario escribe información en el control, se debe actualizar la propiedad del objeto de dominio, *cuidando de no entrar en un loop de notificaciones infinitas hacia la vista y viceversa*.

Toda esta responsabilidad que marcamos anteriormente bien podría ser de los observers, pero dada la complejidad del manejo de eventos en UI es común que esté separado en un objeto que se ocupe exclusivamente de esto: un contexto de binding, objeto responsable de recordar las diferentes parejas de modelo-control que están vinculados entre sí y de repartir los eventos teniendo en cuenta todas las complejidades que eso implica (evitar los ciclos, manejar la concurrencia, etc).

### Cómo se implementa en JFace

En JFace esto se implementa a través de la clase DataBindingContext que conoce a ambos observers: los interesados en los cambios del modelo pero también el modelo que es interesado de los cambios que hace el usuario a través de la vista.

Si queremos tener binding bidireccional entre el atributo nombre de un Socio y el control textbox de la pantalla de actualización de un socio

-   Al generarse la pantalla debemos bindear la propiedad "nombre" del objeto socio con el control textbox de la pantalla de actualización
-   Si se modifica el valor del atributo nombre de un socio (por afuera de la UI), debemos disparar la notificación al textbox. Esto se hace en el setter de nombre, enviando el mensaje firePropertyChange.
-   Si el usuario escribe algo en el textbox, eso dispara automáticamente la actualización del modelo. Ejemplo: en el textbox del nombre escribimos "TARCISO", entonces se disparará el mensaje setNombre("TARCISO") al objeto Socio. El setNombre a su vez disparará un firePropertyChange, pero la lógica de notificación es inteligente y sabe cortar aquí el flujo de avisos para no entrar en loop.

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

### El textbox en JFace

-   Podemos bindear la propiedad text de un control textbox contra un atributo String de un modelo de la siguiente manera

`new` `DataBindingContext().bindValue(` `SWTObservables.observeText(controlTextBoxEnCuestion,` `SWT.FocusOut),` `BeansObservables.observeValue(objetoModelo,` `propiedadDelModeloContraElQueSeBindea),` `null,` `null);`

Otras propiedades que admiten binding bidireccional:

-   Editable: un valor booleano que permite habilitar/deshabilitar el input desde el teclado
-   Visible: el control puede hacerse visible o invisible en base al binding con un atributo de tipo boolean del modelo
-   Font/Foreground/Background: se puede modificar la letra con la que se visualizan los datos cargados en el control, el color de la letra o bien el de fondo, en base a un atributo de un modelo.

Estas propiedades no pueden "observarse", es decir, no tenemos binding bidireccional con un atributo del modelo. No obstante, podemos modificar sus valores al generar el control o bien en algún otro momento:

-   width: el tamaño del control
-   size: la cantidad de caracteres que permite cargar

Control Checkbox
----------------

Dependiendo del framework se los denomina checkbox, button(de tipo check), input (type check), etc.

Es un control pensado para modelar atributos booleanos del modelo (a través de la propiedad value), de forma cuadrada con un tilde marcado o desmarcado según la propiedad sea true/false respectivamente. A los checkboxes se les puede setear por lo general los atributos read only y visible.

Ejemplos que pueden modelarse con este control:

-   un usuario puede estar activo/inactivo
-   una persona puede ser mayor de edad o no
-   un pedido entregado/pendiente (siempre que no existan más estados)

Control Combo
-------------

Dependiendo del framework se los llama combo box, drop down, input (type select), etc.

*Ejemplo:* si modelamos una aplicación para un Videoclub, cada película se relaciona con un género. Entonces la pantalla de carga de una película tiene:

-   un TextBox título
-   un TextBox año de filmación (numérico)
-   un ComboBox que selecciona un género

Cuando el usuario cambia la selección del combo tenemos que enviar el mensaje `pelicula.setGenero(genero);`

El combo necesita tener asociado:

-   un conjunto de elementos "seleccionables"
-   el elemento seleccionado

¿De qué tipo es esa lista de elementos seleccionables? Si trabajamos con generics, podríamos pensar en un Combo<T> de manera que el modelo subyacente sea un Collection/Set/List<T> y el elemento seleccionado un T. En el ejemplo planteado T = Genero.

Otra alternativa menos elegante es que el combo tenga el índice del elemento seleccionado (un int). Por otra parte la lista de elementos del combo podría restringirse a ser solamente una lista de strings que son los que se van a mostrar en pantalla.

Independientemente de la manera en que trabajemos, vamos a necesitar tener un **conversor** para asociar los elementos visuales del combo con los objetos que representan esas opciones y pasar de uno a otro en ambas direcciones, esto es capturar el evento de cambio para mapear el elemento/índice seleccionado con un objeto Genero que es lo que el modelo necesita.

Control Grilla
--------------

Dependiendo del framework se los llama table, grid, etc. Los combos, textboxes, checkboxes se bindean con un atributo, la grilla es un control compuesto en forma de matriz que tiene:

-   como filas un conjunto de elementos
-   como columnas el conjunto de atributos a visualizar de cada elemento

Control Botón
-------------

Dependiendo del framework se los llama button, action, actionButton, command, etc. Se utilizan para disparar eventos de la UI o mapear acciones del negocio.

Otros controles
---------------

-   **TextArea (TextEdit)**: un Text box de varias líneas que permite ingresar una gran cantidad de palabras (Ej: campo "Observaciones" al registrar un Reclamo o un Pedido)
-   **Radio buttons**: alternativa visual al combo box para seleccionar un conjunto acotado de opciones (Ej: FALTA).
-   **Label (TextView)**: se puede bindear el valor para mostrar información (como la fecha de último alquiler de un socio, la antigüedad en años de un empleado o el monto pendiente de una factura) o estar fijo (acompañando los controles de carga de datos: "Fecha de nacimiento", "Nombre", o como observaciones y ayuda para la carga: "Recuerde que debe ingresar nombre o apellido para buscar un socio").
-   **List Boxes**: Es una alternativa al combo box ya que permiten visualizar más de un elemento a la vez. También admiten seleccionar múltiple, aunque respecto a la grilla tienen la desventaja de tener una sola columna.
-   **Trees/Treeviews**: Es un control que permite visualizar nodos en forma jerárquica, suelen trabajarse con algoritmos recursivos. Ej: FALTA.
-   **MessageArea (StatusBar)**: un área para ubicar mensajes de error (o bien se puede manejar con un label)
-   **Containers (Frame/Group/Tab/Panel/Form)**: son controles contenedores de otros controles

Revisar:

A estas validaciones pueden sumarse otras realizadas por el modelo de dominio, por ejemplo en el momento de setearle el valor. Todos estos objetos en conjunto intervienen cada vez que se produce un evento en uno de los dos extremos de la cadena de binding, para hacer llegar el evento (y probablemente el valor asociado) hasta el otro extremo.

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
