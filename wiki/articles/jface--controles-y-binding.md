**Nota:** el objetivo de esta página es comentar cómo se produce el binding entre controles de SWT/JFace y atributos de dominio. Si desea documentación para programar interfaces de usuario puede consultar [la página oficial de SWT](http://www.eclipse.org/swt/) y [el wiki de JFace](http://wiki.eclipse.org/index.php/JFace)

Cómo se implementa el binding en JFace
--------------------------------------

A través de la clase DataBindingContext que conoce a ambos observers: los interesados en los cambios del modelo (BeansObservables) pero también al modelo le interesa los cambios que hace el usuario a través de la vista (SWTObservables).

*Ejemplo:* si queremos tener binding bidireccional entre el atributo nombre de un Socio y el control textbox de la pantalla de actualización de un socio

-   Al generarse la pantalla debemos bindear la propiedad "nombre" del objeto socio con el control textbox de la pantalla de actualización
-   Si se modifica el valor del atributo nombre de un socio (por afuera de la UI), debemos disparar la notificación al textbox. Esto se hace en el setter de nombre, enviando el mensaje firePropertyChange.
-   Si el usuario escribe algo en el textbox, eso dispara automáticamente la actualización del modelo. Ejemplo: en el textbox del nombre escribimos "TARCISO", entonces se disparará el mensaje setNombre("TARCISO") al objeto Socio. El setNombre a su vez disparará un firePropertyChange, pero la lógica de notificación es inteligente y sabe cortar aquí el flujo de avisos para no entrar en loop.

Control Textbox
---------------

En SWT el textbox está representado por la clase `org.eclipse.swt.widgets.Text`

Podemos bindear la propiedad text de un control textbox contra un atributo String de un modelo de la siguiente manera

<code>

`new DataBindingContext().bindValue(`
`    SWTObservables.observeText(controlTextBoxEnCuestion, SWT.FocusOut), `
`    BeansObservables.observeValue(objetoModelo, propiedadDelModeloContraElQueSeBindea), `
`    null, `
`    null);`

</code>

Otras propiedades que admiten binding bidireccional:

-   **Editable**: un valor booleano que permite habilitar/deshabilitar el input desde el teclado
-   **Visible**: el control puede hacerse visible o invisible en base al binding con un atributo de tipo boolean del modelo
-   **Font/Foreground/Background**: se puede modificar la letra con la que se visualizan los datos cargados en el control, el color de la letra o bien el de fondo, en base a un atributo de un modelo.

Estas propiedades no pueden "observarse", es decir, no tenemos binding bidireccional con un atributo del modelo. No obstante, podemos modificar sus valores al generar el control o bien en algún otro momento:

-   width: el tamaño del control
-   size: la cantidad de caracteres que permite cargar

Control Checkbox
----------------

En SWT el checkbox está representado por la clase `org.eclipse.swt.widgets.Button` pasándole el parámetro SWT.CHECK al constructor.

Se puede bindear la propiedad SELECTION contra un atributo boolean de un modelo:

<code>

`new DataBindingContext().bindValue(`
`    SWTObservables.observeSelection(controlCheckBoxEnCuestion), `
`    BeansObservables.observeValue(objetoModelo, atributoBooleanDelModeloContraElQueSeBindea), `
`    null, `
`    null);`

</code>

También se puede bindear las propiedades visible, font, foreground y background, entre otras. No tiene sentido bindear la propiedad text, hacerlo resulta en error.

Control Combo
-------------

En SWT el combo está representado por la clase `org.eclipse.swt.widgets.Combo`

Se agregan elementos al combo:

-   enviando el mensaje add(String string) o add(String string, int index) o setItem(int index, String string)
-   enviando el mensaje setItems(String\[\] items)

Como se desprende de la interfaz de cada uno de los métodos

-   no podemos agregar cualquier elemento, tienen que ser strings, esto nos fuerza a 1) tener en paralelo una lista de elementos "posta" relacionados o 2) recuperar con ese string el elemento (el string debe identificar de manera unívoca a ese elemento). *Ejemplo:* si el combo muestra socios de un videoclub, debemos mostrar por un lado el nombre del socio pero por otra parte tenemos que encontrar el objeto socio "posta" en base al nombre. De la misma manera ocurriría con los géneros de una película, o con los libros de una biblioteca, etc.
-   los elementos se guardan en un orden, por eso existen métodos que agregan a partir de una posición

Para conocer cuál es el elemento seleccionado del combo enviamos el mensaje getSelectionIndex(), que devuelve un int. Vemos el javadoc:

<code>

`public int getSelectionIndex()`
`    Returns the zero-relative index of the item which is currently selected in the receiver's list, or -1 if no item is selected.`

</code>

Tener el selectionIndex como un entero refuerza la idea de que el orden en el combo es importante, si tengo los elementos en un Set no me serviría el selectionIndex.

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

Links relacionados
------------------

-   [Formas de vincular una vista con el modelo de dominio](formas-de-vincular-una-vista-con-el-modelo-de-dominio.html)
-   [Ejemplos de Binding entre vista y modelo](ejemplos-de-binding-entre-vista-y-modelo.html)
-   [Algo3 Temario](algo3-temario.html)

