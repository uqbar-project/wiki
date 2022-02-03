---
layout: article
title: Tutorial de squeak y pharo
---

Empezar a usar el Squeak puede ser un poco complicado porque no es muy intuitivo, les dejamos una explicación para poder arrancar con menos miedo. Pharo no es muy diferente, así que sólo aclaramos cuando difiere para no armar todo de nuevo :D

Crear una Clase
---------------

1) Abrir el Class Browser (System Browser): Hagan click sobre el fondo del squeak para que se les abrá el menú World -&gt; open... y eligen Class Browser (en Pharo se llama System Browser)

2) En la primer columna del Class Browser tienen las categorías, allí deberían crear una nueva para agrupar las clases que vayan haciendo. Para eso hacen click derecho sobre esa columna y eligen add item... (en Pharo es Add Category) Les va a pedir que ingresen el nombre para la categoría, por ejemplo, TPObjetos-grupoBle (no sé si había algún formato requerido, pero pongan algo que los identifique).

Al darle OK les va a aparecer el template para crear clases dentro de esa categoría:

`   Object subclass: #NameOfSubclass`
`       instanceVariableNames: ''`
`       classVariableNames: ''`
`       poolDictionaries: ''`
`       category: 'TPObjetos-grupoBle'`

3) Crear clases: Siempre que quieran crear una clase nueva hacen click sobre el nombre de la categoría para que les aparezca el template anterior. Lo que tienen que hacer es cambiar NameOfSubclass por el nombre real, por ejemplo, Pelicula; después para agregarle variables de instancias tienen que escribir los nombres entre las comillas simples de instanceVariableNames separadas por espacios. Si tuvieran variables de clase irían en el renglón de abajo. Recuerden que las de instancia empiezan con minúscula y las de clase con mayúscula por convención. Finalmente, si quisieran que herede de otra clase que no sea Object, cambian Object por el nombre de la superclase.

Para dar los ejemplos vamos a usar la parte más básica de un parcial/tp "Salas de Cine":

*Se desea hacer un sistema para una cadena de cines, en la cual guardar la información de las salas que administra la cadena y las películas que se dan en cada horario en cada una de esas salas. Para simplificar asumimos que todas las películas comienzan en punto y que duran una cantidad entera de horas. Además de su duración, de cada película se sabe su género.*

`   Object subclass: #Pelicula`
`       instanceVariableNames: 'duracion genero titulo'`
`       classVariableNames: ''`
`       poolDictionaries: ''`
`       category: 'TPObjetos-grupoBle'`

Una vez que completaron el template hacen click derecho -&gt; Accept o simplemente Ctrl+S.

Si quieren editar la definición de la clase, por ejemplo para agregar/quitar variables o cosas así, clickeando sobre el nombre de la clase debería mostrarles el código que guardaron y vuelven al paso 3.

Para crear una subclase pueden cambiar el template para que use la superclase en vez de Object. Más detalles, acá: [Cómo crear una subclase en Squeak](como-crear-una-subclase-en-squeak.html)

Métodos
-------

### Creando un Método

4) Agregar métodos: Haciendo click sobre la 3er columna (donde tienen --all-- y no messages) les aparece un ejemplo de cómo quedaría un método:

`   message selector and argument names`
`       "comment stating purpose of message"`
`       | temporary variable names |`
`       statements`

Eso lo borran y escriben el método que tengan ganas. Por ejemplo:

`   verificarLongitud`
`       "Esto de abajo es fruta, pero para que se imaginen cómo podría escribirse un método"`
`       ^ self genero nombre size > 10 ifTrue: [30] ifFalse: [20]`

Acá se están usando 2 mensajes aún indefinidos: genero y nombre, que son los getters de las variables de instancia. Los accessors se pueden crear directamente haciendo click derecho en el nombre de la clase y... En Squeak: -&gt; more... -&gt; create inst var accessors En Pharo: -&gt; Refactor Class -&gt; Accessors (para crear todos los que falten)

`         -> Refactor instance variable -> Accessors (para elegir una variable para cual crear los accessors)`

Como todavía no había hecho eso al hacer Ctrl+S al método verificarLongitud para aceptar y que se agregue efectivamente debería aparecer un cartelito diciendo que no conoce esos selectores porque no hay ninguna clase que defina genero y nombre, entonces les tira un par de opciones por si se confundieron, siendo la primera lo que tipearon, las siguientes algunas alternativas existentes y al final Cancel para seguir editando el método sin aceptar los cambios.

Mucho más que eso no hay para lo que es definir las clases. Lo que quedaría es armar el programita -&gt; workspace (no main =P). Lo que tiene de copado es que tienen un ambiente vivo y pueden mandarles mensajes a los objetos como se les de la gana, no es necesario tener todo el programa armado y darle run, lo pueden ir armando a la par que le agregan los métodos a las clases. Así que...

### Modificando un Método

Para modificar un método, si se equivocaron, pueden hacerlo seleccionando el método, haciendo los cambios y apretando aceptar. Pero ¡Ojo! ''' No se puede modificar así nomás el nombre de un método. ''' Si se quiere cambiar su **selector** (cualquier cosa del nombre ó los parámetros) deberán hacerlo así:

-   Clck en la lista de métodos sobre el que quieren modificar
-   Botón derecho en el método -&gt; **refactor method**

Y ahí les ofrece varias cosas, eligen lo que quieren hacer.

Escribir Un Workspace
---------------------

5) El Workspace: Volvemos al fondo del Squeak, hacen click para que aparezca World -&gt; open... -&gt; workspace

Ahí les aparece un cuadro de texto en blanco para que tiren código. Lo primero que podemos hacer es instanciar una Pelicula y settearle un par de atributos. Así que escribimos lo siguiente en el workspace:

`   saw := Pelicula new.`
`   saw titulo: 'El Juego del Miedo'.`
`   saw duracion: 2.`

Si seleccionan esas tres líneas (para seleccionar todo tienen Ctrl+espacio) y hacen Ctrl+d (do it) se evalua ese código. Para ver si anduvo todo bien pueden escribir lo siguiente en el workspace:

`   saw titulo.`

Si se paran al final de ese renglón (para evaluar una sola línea no es necesario seleccionar, si seleccionan todo se va a evaluar todo desde el principio nuevamente y eso no tiene gracia, jeje) y le dan Ctrl+p (print it) les va a aparecer al final de la línea y seleccionado para su conveniente borrado el string 'El Juego del Miedo'

Otra forma de ver qué onda con nuestros objetitos es pararse sobre la variable con la que lo referenciamos, en este caso saw, y hacer ctrl+i (inspect it). Eso les abre una ventanita donde pueden ver a qué está referenciando cada una de las variables. En este caso tendrían lo siguiente:

`   `**`self` `->` `a` `Pelicula`**
`   `**`all` `inst` `vars:`**
`   `**`duracion:` `2`**
`   `**`titulo:` `'El` `Juego` `del` `Miedo`**`'`
`   `**`genero:` `nil`**

Claro, el genero está en nil porque no le setteamos nada, de hecho ni siquiera creamos la clase Genero. Hagamos eso así probamos verificarLongitud...

Creamos la clase Genero dentro de la categoría TPObjetos-GrupoBle con las v.i. que quieran, incluyendo nombre. Después create inst var accessors y listo.

Volvemos al workspace... agregamos el siguiente código:

`   terror:=Genero new.`
`   terror nombre: 'Terror'.`
`   saw genero: terror.`
`   `
`   saw verificarLongitud.`

Seleccionamos ese código y le damos ctrl+p y al final de ese código seleccionado aparece el número 20 que es lo que esperábamos.

Métodos de Clase
----------------

5) Métodos de clase: supongamos que el género lo quieren instanciar directamente como:

`   terror := Genero conNombre: 'Terror' yDuracionMaxima: 1.5.`

Ahí habría que escribir el método de clase conNombre: yDuracionMaxima: Para eso vamos a la clase Genero en el Class Browser y hacemos click en el botoncito que dice class (hasta ahora veníamos trabajando del lado de las instancias). De manera análoga, hacemos click en la tercer columna para que nos aparezca el ejemplo de método y escribimos lo siguiente:

`   conNombre: unNombre yDuracionMaxima: unaDuracion`
`   ^ self new`
`           nombre: unNombre;`
`           duracionMaxima: unaDuracion.`

Ahí lo único loco que hice fue usar un truquito que no contamos en clase me parece que es el de enviar mensajes en cascada. La forma de evaluación de ese código sería: primero self new por ser unario, eso me da una instancia de Genero, a esa instancia se le envía el mensaje nombre: con el parámetro que nos llegó de afuera unNombre. El punto y coma indica que la siguiente sentencia es un mensaje que se le envía al mismo objeto de antes, o sea al genero que instanciamos recién. Le mandamos duracionMaxima: y le setteamos la duración correspondiente.

Finalmente retorna al objeto ya inicializado (el ^ es lo que tiene menor precedencia) y lo que va a retornar es lo que haya retornado el último envío de mensajes. Como duracionMaxima: es un setter por defecto que no tiene ningún ^ estamos seguros de que lo que va a retornar (porque TODOS los mensajes retornan un objeto) es al objeto receptor.

A modo informativo, si duracionMaxima: retornara algo diferente de self habría que hacer lo siguiente:

`   conNombre: unNombre yDuracionMaxima: unaDuracion`
`       ^ self new`
`               nombre: unNombre;`
`               duracionMaxima: unaDuracion;`
`               yourself.`

Para que nos retorne al objetito receptor.

La otra alternativa que a mí me resulta medio incómoda pero es la más fácil es definirle un mensaje de instancia al género que sea nombre: yDuracionMaxima: que le settee internamete las dos variables al objeto. Con lo cual quedaría lo siguiente:

`   conNombre: unNombre yDuracionMaxima: unaDuracion`
`   ^ self new nombre: unNombre yDuracionMaxima: unaDuracion.`

Y del lado de las instancias (hacemos click en instance) agregamos el método nombre: yDuracionMaxima: que haga lo siguiente:

`   self nombre: unNombre.`
`   self duracionMaxima: unaDuracion`

Y no retornamos nada porque queremos que nos retorne self.

Ahora podemos refactorizar nuestro workspace y nos quedaría algo así:

`   saw := Pelicula new.`
`   saw titulo: 'El Juego del Miedo'.`
`   saw duracion: 2.`
`   `
`   terror:=Genero conNombre: 'Terror' yDuracionMaxima: 1.5.`
`   saw genero: terror.`
`   `
`   saw verificarLongitud.`

Hacemos Ctrl+space + Ctrl+p y mágicamente sigue dando 20 =D O sea que hicimos las cosas bien y no rompimos nada.

Guardando nuestro trabajo
-------------------------

### Grabar la Imagen

6) Guardar los cambios: Hagan click en el fondo para abrir World -&gt; save

### Grabar en un archivo .st

7) FileOut: Para hacer la entrega tienen que hacer un fileOut de la categoría. Para eso sólo le dan click derecho al nombre de la categoría -&gt; fileOut. Cuando yo lo hice no me preguntó en dónde guardar ni con qué nombre, simplemente se guardó en mi directorio de instalación del Squeak con el nombre de la categoría y extensión .st

No se preocupen si ven muchos !!!! en el archivo, es lo que tienen que pasar. Después de todo no fue hecho para ser leído por mortales, nada más para ser importado en otro Squeak o para los paspados que prefieren la entrega en papel, jajajaj (mentira, es útil tenerlo en papel, y con un poquito de cancha es hasta fácil de seguir)

### Grabar el Workspace

8) Guardar el Workspace: obviamente el fileOut sólo les guarda el código de las clases que estén en la categoría, el ws hay que guardarlo aparte. Pueden hacer simplemente copy/paste y pegarlo a un txt normal o pueden usar la opción de guardado que trae el Squeak. Para eso: click derecho sobre el workspace -&gt; more... -&gt; save contents to file

Le ponen el nombre que quieran y la extensión la pueden dejar como txt o cambiarla a st, que es como se guarda en la otra versión de Smalltalk. Se los va a guardar en la misma carpeta por default en la que se hizo el fileOut.

### Importar un archivo .st

-   Arrastrar el archivo .st y soltarlo sobre la imagen de Pharo. Le dan **FileIn Entire File** y ¡Listo!

<!-- -->

-   Otra forma:

  
1) Botón derecho sobre el fondo. En el menú **World** van a **Tools -&gt; File Browser**.

2) En el File Browser van a ver tres paneles:el de la izquierda muestra el arbol de directorios y carpetas de su sistema.

3) Seleccionar la carpeta donde esta instalado Pharo, que es donde va a estar el archivo .st que quieren importar. En el panel de la derecha, van a ver la lista de los archivos incluidos en esa carpeta.

4) Seleccionen el archivo .st. En el panel inferior van a ver el codigo que escribieron, y arriba de los paneles superiores van a aparecer varios botones, elijan **Install**

Shortcuts / Atajos del Teclado / Fatalities
-------------------------------------------

-   **Ctrl+b** seleccionando una clase\* para que se abra un Class Browser sobre esa clase.
-   **Ctrl+n** seleccionando un selector\* para ver sus Senders (se abre una lista de métodos desde donde se envía ese mensaje, también vale para cuando se usa un symbol igual al mensaje, onda \#size).
-   **Ctrl+m** seleccionando un selector\* para ver sus Implementors (se abre una lista de métodos para ese mensaje)
-   **Ctrl+shift+n** sobre una clase\* para ver en dónde se usa (References)
-   **Ctrl+i** sobre un objeto nos permite inspeccionarlo por adentro
-   **Alt+punto** para mandar una interrupción (útil para cuando uno se la manda y queda ejecutando algo recursivamente, se interrumpe la ejecución y se puede debuggear el stacktrace desde donde interrumpimos)

\* En cualquiera de estos casos, "seleccionar" se puede entender como seleccionar un selector / nombre de clase dentro de un cacho de código (workspace, definición de un método en el class browser, el debugger, etc), o bien seleccionarlo en un listado de cualquier browser (el Class Browser, la lista de variables de instancia y temporales que aparece en un debugger, una lista de senders que hayas abierto, etc).

Por cierto, todas las cosas que se listan con Ctrl también funcionan con Alt en Windows (a veces sólo funcionan con Alt).
