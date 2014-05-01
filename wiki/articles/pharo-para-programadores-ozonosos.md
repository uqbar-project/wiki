La migración desde trabajar con Ozono a pasar a la versión full de Pharo puede ser complicado en ciertos sentidos pero más o menos es lo mismo pero tenemos que tener en cuenta las diferencias.

Clases vs. Objetos
------------------

La primer diferencia que tenemos en este nuevo ambiente es que Pharo esta centrado en las clases y no en los objetos.

Como ya vimos en las clases teóricas, los objetos ya no los vamos a crear uno por uno sino que vamos a crear los objetos a partir de clases.

Por lo tanto, la forma que tiene Pharo (y muchos otros lenguajes orientados a objetos) es organizarse alrededor clases.

System Browser, el centro de nuestro desarrollo
-----------------------------------------------

Para comenzar vamos a necesitar una imagen de Pharo, cualquier imagen nos va a servir; pueden seguir usando la que tiene instalada Ozono :D.

Nuestro principal amigo en el desarrollo con Pharo va a ser el *System Browser*. Este lo encontramos dentro del menú principal de Pharo.

![](PharoParaOzonosos-World.png "PharoParaOzonosos-World.png")

Esto nos va a abrir la siguiente ventana (es bastante compleja para decirle ventanita).

![](PharoParaOzonosos-SystemBrowser.png "PharoParaOzonosos-SystemBrowser.png")

### Secciones del System Browser

Esta ventana se divide en distintas secciones, esta ventana es central, presenta mucha mucha mucha información por eso vamos a analizarla por secciones.

![](PharoParaOzonosos-SystemBrowserSections.png "PharoParaOzonosos-SystemBrowserSections.png")

Estas Secciones son:

-   **Categorias de clases:** en esta sección aparece una forma de categorizar las clases. Las categorías de las clases en Pharo

se llaman Packages, estos paquetes de clases pueden contener una o más subcategorías dentro.

-   **Clases:** una vez que selecciono un paquete se me van a mostrar las clases que están definidas en ese paquete. Porque algunas

aparecen con signo de admiración (!), porque no tienen escrita la documentación que explica que hace la clase.

-   **Categorías de Métodos:** cuando selecciono una clase se me van a mostrar los métodos que tiene definida esa clase. Estos

métodos pueden estar clasificados en distintas categorías, esto es muy útil como forma de documentación y suma mucho valor cuando comienza a crecer mi programa.

-   **Metodos:** al ir navegando las categorías de los métodos van a ir mostrándome la lista de métodos de esa clase en la categoría

seleccionada.

-   **Área de Edición de Código:** En esta área de la ventana es donde se realiza la edición del código. Si elegimos un método vamos a

ver el código de ese método, pero si elegimos una clase nos muestra un código raro. No entremos en pánico, ese código es la definición de la clase, ya vamos a entrar en el detalle de como hacerlo.

Por ahora debemos entender las distintas secciones del *System Browser* pronto vamos a ver para que se usa cada cosa con un ejemplo.

Workspaces
----------

Bueno, bueno, todo muy bonito pero a mi me gustaba enviar mensajes a mis objetos. Esto lo perdimos, esto es una mierda. No, no perdimos nada!!!

La forma de enviar mensajes y probar nuestro programa van a ser nuestros bonitos workspace (después vamos a hablar de tests, pero eso es otra versión mucho más cheta aunque menos interactiva).

![](PharoParaOzonosos-Workspace.png "PharoParaOzonosos-Workspace.png")

Para crear un workspace lo hacemos desde el menú principal de Pharo, es el mismo que usamos inicialmente.

Además podemos tener muchos workspace abiertos.

Los workspace de Pharo se usan de la misma manera que los de Ozono que ya usamos previamente. Pero con una variación interesante, podemos definir variables dentro del workspace. Estas variables son locales al workspace. Para definirlas debemos usarlas directamente, asignandole un valor. Cuando evaluemos esa línea va a crearse la variable.

Atención, los workspace no se van a guardar solitos como en Ozono, tenemos que guardarlos a mano. Normalmmente no nos vamos a preocupar por esto ya que el código que pongamos en el workspace es para pruebas rápidas. Si queremos guardar algo que vamos a probar varias veces nos sirve la idea de Tests, ya la vamos a ver; pero por ahora con el workspace nos sirve.

Los workspace se guardan como archivos de texto con extensión *.ws*, para poder guardar y cargar estos archivos tenemos que usar la flechita que esta a la derecha y arriba del workspace.

![](PharoParaOzonosos-Workspace-Menu.png "PharoParaOzonosos-Workspace-Menu.png")

Un ejemplo Paso a Paso
----------------------

Para poder mostrar como se usa Pharo, vamos a implementar el siguiente ejemplo.

Vamos a tener que construir un sistema para administrar camiones en una empresa de logística. De cada camión vamos a saber su capacidad en kilos y su carga actual, y además podemos saber si el camión esta lleno o no.

Al analizar y plantear una solución al problema, identificamos que todos los objetos camiones tienen el mismo comportamiento; por lo que extrajimos la siguiente clase:

![](PharoParaOzonosos-Camion.png "PharoParaOzonosos-Camion.png")

Tenemos que recordar que la solución se da con los objetos que representan los camiones, las clases sirven para crear muchos camiones iguales, o sea se comportan todos juntos.

Recordemos un poquito la forma de comunicación con los diagramas de clase. Este rectángulo que representa la clase tiene 3 secciones. En la superior va el nombre de la clase. En la del medio los distintos atributos que tienen los objetos creados con esta clase. Y en la inferior los mensajes que entienden los objetos creados con la clase.

Perfecto ya sabemos que es lo que tenemos que hacer, ahora vamos a implementarlo en Pharo!

### Creación de Paquetes

Para comenzar tenemos que crear un paquete nuevo dentro de Pharo. Todas las clases de Pharo estan definidas dentro de un paquete, es muy recomendable que creemos un paquete para cada uno de los ejercicios o problemas o sistemas que creemos.

Para crear un paquete tenemos que hacer click derecho sobre el sector de paquetes del System Browser y elegir la opción *Add Package...*. Esta opción nos va a preguntar el nombre del nuevo Paquete.

Podemos ponerle el nombre que queramos, pero existe una convención que se usa en todos lados y que Pharo va a usar para presentarnos la información. Esta convención es separar las palabras con guión del medio.

En este caso vamos a crear el paquete **Logistica**.

Una aclaración importante, es que aunque es probable que Pharo se banque nombres con acentos o ñ o cualquier caracter UTF-8, a los programadores nos gusta obviar los acentos y las ñ; porque siempre siempre siempre van a traer problemas.

Cuando creamos el paquete nos debería quedar algo así:

![](PharoParaOzonosos-PaqueteCreado.png "PharoParaOzonosos-PaqueteCreado.png")

### Creación de Clases

Ya tenemos nuestro paquete creado, por ahora esta vació y triste y solo; por lo que vamos a comenzar creando la clase Camión.

Para crear una clase tenemos que hacer las siguientes operaciones:

1.  Hacer click sobre el paquete donde va a vivir nuestra nueva clase.
2.  Ingresar la definición de la clase en el *Área de Edición de Código*, ahora vamos a hablar de que hay que escribir ahí.
3.  Aceptar la definición, ya sea haciendo Ctrl+S o haciendo botón derecho y darle la opción Accept.

#### Definición de una Clase

Las clases en cualquier ambiente de Smalltalk se definen a partir de un envió de mensaje. Nadie en la vida se acuerda este mensaje que hay que mandar a la clase Object, pero al hacer click sobre un paquete la herramienta ya nos propone un template:

`  Object subclass: #NameOfSubclass`
`         instanceVariableNames: ''`
`         classVariableNames: ''`
`         category: 'Logistica'`

La definición de nuestra clase Camion es la siguiente:

`  Object subclass: #LgCamion`
`         instanceVariableNames: 'capacidad carga'`
`         classVariableNames: ''`
`         category: 'Logistica'`

Si se fijan no es tan tan loca.

Las cosas que voy a modificar son:

-   El nombre de la clase, es eso que esta después del numeral
-   Las variables de instancia, acá agrego dentro del string cada uno de los nombres de las variables de instancia separados por un espacio.

En este caso tengo dos variables de instancia, capacidad y carga.

-   Nombre de la categoría donde esta clase, acá ya me pone la que corresponde al paquete donde estoy parado, pero siempre lo puedo cambiar.

Un minuto cerebrito, acá dijimos que ibamos a crear la clase Camion y vos le pusiste LgCamion; me estas cargando o todas las clases comienzan con Lg???

Ninguna de las dos cosas, es un prefijo que identifica a nuestro paquete; como cree un paquete llamado Logistica definí que el prefijo que identifique a las clases dentro de este con el prefijo Lg.

Esto lo tenemos que hacer porque Pharo no tiene espacios de nombres independientes, o sea no puedo tener dos clases con el mismo nombre, si quisiera crear una clase con el mismo nombre de otra que existe, voy a pisar la original. Por eso vamos a generar nombres únicos teniendo un prefijo para el paquete y el nombre de la clase.

Si estamos muy muy muy seguros que no existe otra clase que se llame Camion, podría ponerle ese nombre. Pero yo debo contarle el estandard.

Después de crear la clase nos debería quedar algo así:

![](PharoParaOzonosos-ClaseCreada.png "PharoParaOzonosos-ClaseCreada.png")

Atención, no nos preocupemos por ese signo de admiración rojo. Ya lo dijimos, pero lo recuerdo, es un indicativo que la clase no esta documentada. Nada importante, por ahora, es algo que nos debería importar.

#### Definición de Métodos

Ya definimos nuestra clase ahora debemos agregarle métodos.

Para poder agregar un método a una clase, tenemos que tener seleccionada la clase donde lo vamos a definir y hacer click sobre la categoría del método que vamos a generar; si no tenemos la categoría, la podemos crear o hacer click en **no messages** que es la que tengo cuando esta vacía o en **-- all --** si ya tiene alguna.

Cuando hacemos esto la sección de edición de código nos va a ofrecer el template, esto no es solo necesario para que nos muestre el template, sino que lo tengo que hacer para que Pharo sepa donde voy a meter el método.

![](PharoParaOzonosos-CreandoUnMetodo.png "PharoParaOzonosos-CreandoUnMetodo.png")

Entonces vamos a escribir el código de nuestro primer método, lo que vamos a hacer es un mensaje para que el camión nos devuelva su capacidad:

`      capacidad`
`          ^ capacidad`

Para guardar el método creado, tenemos que hacer CTRL+s o botón derecho y elegir la opción **Accept**.

Cuando hacemos esto, nos queda el siguiente resultado.

![](PharoParaOzonosos-MetodoCreado.png "PharoParaOzonosos-MetodoCreado.png")

#### Probando nuestros Objetos

Ya tenemos creada nuestra clase, ahora probemos los objetos que creamos. Para poder crear objetos y mandarles mensajes necesitamos usar un workspace.

En un nuevo workspace vamos a escribir el siguiente código.

`      unCamion := LgCamion new.`
`      unCamion capacidad.`

Podemos ir ejecutando línea a línea. La primera nos conviene ejecutarla con un Do-It (o haciendo Ctrl+d), ya que es una línea que tiene efecto de lado. Esta línea crea un nuevo objeto a partir de la clase **LgCamion** y lo asigna a la variable local **unCamion**.

La 2da línea la vamos a ejecutar con un Print-It (o haciendo Ctrl+p), ya que es una expresión que va a devolver la capacidad del camión.

Dando como resultado lo siguiente:

![](PharoScreenshot.7.png "PharoScreenshot.7.png")

Pero porque cuando evaluamos la 2da expresión nos devuelve **nil**? Bueno, porque todas los atributos de un objeto arrancan en nil.

Por eso debemos agregarle el método para poder establecer los valores de los objetos. Vamos a agregar los siguientes métodos a la clase **LgCamion**:

`     capacidad: unValor.`
`       capacidad := unValor.`

`     carga`
`       ^ carga.`

`     carga: unValor`
`       carga:=unValor.`

Además vamos a definir el método para responder al mensaje **estasLleno**.

`     estasLleno.`
`       ^ capacidad = carga.`

Entonces una vez que tenemos este mensaje podemos hacer una prueba más interesante con el siguiente workspace.

`     unCamion := LgCamion new.`
`     unCamion capacidad:1000. `
`     unCamion carga:1000.`
`     unCamion estasLleno.`

Este workspace ya nos permite probar una variante, que podemos ir modificando para probar nuestros objetos. Una alternativa interesante para esto, es poder crear tests unitarios.

### Como guardar mis programas

En Pharo, los programas se guardan dentro de la imagen, o sea tengo que ir guardando la imagen con el código y los objetos de mis programas.

#### Exportar a un .st

Además puedo exportar los paquetes como un archivo *.st*. Para realizar esto podemos hacerlo desde el **System Browser** y con el botón derecho sobre un paquete y eligen la opción **File Out**.

Al hacer esto se genera un archivo con el nombre del paquete y la extensión **.st** que incluye todo el código fuente de las clases del paquete.

#### Importar un .st

Para poder importar un archivo **.st** lo más fácil es arrastrar el archivo sobre una imagen de Pharo que se encuentre corriendo y al soltarlo elegir la opción **FileIn Entire File** y listo ya lo va a cargar en la imagen.
