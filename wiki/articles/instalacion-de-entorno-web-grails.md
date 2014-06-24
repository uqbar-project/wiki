Pre-requisitos
--------------

Asumimos que tenés instaladas las herramientas básicas, en particular necesitaremos que tengas una JDK, no puede ser una JRE, y recomendamos que uses 1.7 ó superior. Para más ayuda, seguí este [link](preparacion-de-un-entorno-de-desarrollo-java.html).

GGTS: Entorno integrado de Desarrollo y Framework Grails
--------------------------------------------------------

Recomendamos descargar la última versión estable del GGTS (que a julio del 2014 es la 3.5.1) en el siguiente link

-   <https://spring.io/tools/ggts/all>

Allí seleccionás el entorno adecuado para tu sistema operativo.

### Configuraciones

1.  Es preferible que el directorio de instalación no tenga espacios (C:\\GGTS,o C:\\IDEGrails)
2.  Recordamos que no es válido asociarle una JRE sino que tiene que incluir el compilador y demás herramientas de desarrollo que vienen con la JDK.
3.  La JDK y el GGTS deben coincidir en la versión de 32 ó 64 bits (si el GGTS es 32 bits tenés que apuntar a una JDK de 32 bits, o no va a funcionar)
4.  Para asociar la JDK al GGTS: si descargás y ejecutás el instalador lo definís ahí mismo, si descomprimís el archivo o se lo querés cambiar creás un acceso directo al STS:

`STS -vm "/path donde está la JDK"`

-   las comillas son importantes si en el path hay espacios en blanco, y
-   tené en cuenta incluir el directorio bin del JDK en el path.

Más información en <http://grails.org/products/ggts>

### Primeros pasos

-   La primera pantalla que aparece es el Dashboard (si no aparece, navegar por la barra de herramientas hasta encontrarla), ir a la solapa Extensions. Seleccionar
    -   Subversive (integración de SVN con el IDE)

y luego presionar el botón Install.

### Web Server

El IDE trae consigo un Web Server Tomcat (llamado vFabric tcServer) integrado con el entorno.

Configuración del Workspace
---------------------------

Si estás en Windows te recomendamos que uses un directorio raíz: C:\\WorkspaceGrails o similar (no C:\\Documents and Settings\\User\\etc.)

Configuración del entorno Grails
--------------------------------

-   Primero, seguir las configuraciones estándares para cualquier eclipse como se muestra en [este link](preparacion-de-un-entorno-de-desarrollo-java-configuraciones-adicionales.html)
-   Luego, ir a Window-&gt;Preferences, y filtrar por la palabra "Grails". Seleccionar al nodo "Grails", y en la parte derecha en la sección "Grails Installations", agregar una nueva apuntando a la versión de Grails en el directorio donde se instaló (Groovy &gt; Grails y hacer Add... Browse y buscar la carpeta correspondiente al paso 1)
-   si estás trabajando en un proxy tenés que configurar variables en el Grails Launch (TODO: Ver configuración Telecom)

Si no quieren usar Eclipse
--------------------------

Después buscar un IDE de su agrado e incorporarle el plugin para Grails, acá tienen un link para integrarlo con IDEA: <http://grails.org/IDEA+Integration>

Cómo crear un proyecto desde cero
---------------------------------

Muy simple, New &gt; Grails project.

Cómo empezar
------------

-   Tutoriales para conocer la tecnología: <http://grails.org/start>
-   Manual de referencia: <http://grails.org/doc/latest/guide/>
-   Plugins que se pueden instalar: <http://grails.org/plugins/>

Cómo levanto las aplicaciones
-----------------------------

-   Ctrl + Shift + Alt + G &gt; run-app va a levantar el servidor en el proyecto sobre el cual están parados (eso se puede cambiar en la consola)
-   Una vez que aparezca un mensaje equivalente a

pueden copiar la URL e ir a un Browser y pegar esa dirección para probar la aplicación.

También pueden dar un click sobre la URL y eso los llevará por defecto a un browser interno, que consume bastantes recursos. Por eso recomendamos modificar la configuración default para que les abra un browser por afuera del entorno: Window &gt; Web Browser &gt; Default system web browser.

Hay una solapa Server en el cual pueden agregar o eliminar las aplicaciones web. No obstante este server requiere una configuración adicional para asignarle un Tomcat en forma manual (no es el Tomcat interno que trae el entorno STS de Grails), por lo que por el momento recomendamos no utilizarlo.

Troubleshooting
---------------

### Compatibilidad de las versiones de Grails y JDK

Asegurate de respetar la misma versión de 32/64 bits de tu JDK/GGTS, de lo contrario cuando crees un nuevo proyecto Grails te puede aparecer un mensaje indicando que la JDK asignada corresponde a una JRE (no encontrará 'tools.jar')

### Problemas con Groovy Object al compilar

Al bajar ejemplos puede haber un problema con la versión del compilador ("Groovy Object not found" o si avisa que hay errores con el Build path), en ese caso hay que hacer botón derecho sobre el proyecto &gt; Properties &gt; Groovy compiler y seleccionar "I don't care" en la opción "Groovy compiler level".

### Integración con JDK

-   Grails 2.2.2/2.2.3 y la versión 1.7.0\_25 tienen problemas temporales de integración, si te aparece este mensaje al compilar/correr la app

`Could not determine Hibernate dialect for database name [H2]!`

reemplazá la versión de tu JDK por una distinta.

### Problemas al querer abrir la ventana de comandos

Si al querer abrir la ventana de comandos te aparece una ventana de error con el siguiente mensaje:

`An internal error occurred during: "Retrieving available scripts".`
`java.lang.NullPointerException`

hay que configurar la variable de sistema GRAILS\_HOME para que apunte al mismo path donde estás corriendo desde el IDE (Tip: si es una instalación de GGTS fijate el directorio de instalación + grails-x.y.z donde x,y,z es el número de versión que estás usando actualmente).

Otra opción es que estás trabajando en un proyecto que tiene una versión de Grails que no está instalada en tu máquina, las opciones son

1.  Instalar esa versión de Grails en un directorio diferente (en <http://grails.org/download>)
2.  O bien migrar el proyecto a la versión de Grails que tenés instalada en tu máquina

### Problemas para levantar el IDE con proyectos pesados

No debería ocurrir, ya que los ejemplos son didácticos y están pensados para levantar en entornos sin mayores problemas, pero si el IDE está ocupando mucha memoria al iniciar conviene chequear este link: <http://stackoverflow.com/questions/13515704/how-to-fix-groovy-grails-tool-suite-3-extreme-memory-usage>

Integración con otros lenguajes de programación
-----------------------------------------------

-   [Integración Grails con Xtend](integracion-grails-con-xtend.html)
-   [Integración Grails con Scala](integracion-grails-con-scala.html)
-   Grails se integra naturalmente con Java, sólo hay que definir las clases Java en el source folder src/java y se incluyen con la aplicación web

