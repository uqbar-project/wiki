Pre-requisitos
--------------

Asumimos que tenés instaladas las herramientas básicas, en particular necesitaremos que tengas una JDK, no puede ser una JRE, y recomendamos que uses 1.7 ó superior. Para más ayuda, seguí este [link](preparacion-de-un-entorno-de-desarrollo-java.html).

Framework Grails
----------------

Recomendamos descargar la versión 2.2.2 del siguiente link

-   <http://grails.org/download>

y luego seguir las instrucciones que figuran en el sitio (link Installation).

**Importante:**

1.  Si tu instalación es Windows, no descargues Grails en "Archivos de programa" u otros directorios del sistema. Recomendamos que tengas un directorio raíz "Grails" en el C: o en el D: y de ahí tengas un subdirectorio "Grails 2.2.2", otro "Grails 2.2.3" y así por cada instalación.
2.  La última versión 2.3.0 cambió la forma de manejar las dependencias, no te recomendamos bajarla hasta que estabilicemos los ejemplos.

Entorno integrado de desarrollo (IDE)
-------------------------------------

Si bien se pueden descargar varios plugins para eclipse, recomendamos bajarse el Spring Toolkit Suite del sistema operativo que tengan en

-   <http://www.springsource.org/downloads/sts-ggts> (**IMPORTANTE**: elegir SPRING TOOL SUITE, no GROOVY/GRAILS TOOL SUITE ya que si bien define una configuración básica, no permite jugar con distintas versiones de Grails/Groovy, entre otras cosas)
    -   **Nota:** la versión Eclipse Juno es más liviana, las versiones más nuevas pueden resultar pesadas dependiendo de cada máquina.
    -   Más información en <http://grails.org/products/ggts>

El IDE trae consigo un Web Server Tomcat integrado con el entorno (aunque por cuestiones de performance les recomendamos que cuando prueben abran un browser que no esté embebido con el entorno de desarrollo sino que lo corran por fuera del mismo).

### Completando la instalación

-   Conviene crear un acceso directo al STS especificando a qué JDK apuntar:
    -   incluir el directorio bin del JDK en el path
    -   las comillas son importantes en caso de haber espacios en el directorio

`STS -vm "/path donde está la JDK" `

-   La primera pantalla que aparece es el Dashboard (si no aparece, navegar por la barra de herramientas hasta encontrarla), ir a la solapa Extensions. Seleccionar
    -   Grails support
    -   Groovy-Eclipse
    -   Subversive (integración de SVN con el IDE)

y luego presionar el botón Install.

Configuración del Workspace
---------------------------

Si estás en Windows te recomendamos que uses un directorio raíz: C:\\WorkspaceGrails o similar (no C:\\Documents and Settings\\User\\etc.)

Configuración del entorno Grails
--------------------------------

-   Primero, seguir las configuraciones estándares para cualquier eclipse como se muestra en [este link](preparacion-de-un-entorno-de-desarrollo-java-configuraciones-adicionales.html)
-   Luego, filtrar por la palabra "Grails" y apuntar la versión de Grails en el directorio donde se instaló (Groovy &gt; Grails y hacer Add... Browse y buscar la carpeta correspondiente al paso 1)
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

Troubleshooting
---------------

### Versiones de Grails

-   Te recomendamos que uses Grails 2.2.x ó superior
-   Asegurate de respetar la misma versión de 32/64 bits de tu JDK/STS, de lo contrario cuando crees un nuevo proyecto Grails te puede aparecer un mensaje indicando que la JDK asignada corresponde a una JRE (no encontrará 'tools.jar')

### Problemas con Groovy Object al compilar

Al bajar ejemplos puede haber un problema con la versión del compilador ("Groovy Object not found" o si avisa que hay errores con el Build path), en ese caso hay que hacer botón derecho sobre el proyecto &gt; Properties &gt; Groovy compiler y seleccionar "I don't care" en la opción "Groovy compiler level".

### Integración con JDK

-   Grails 2.2.2/2.2.3 y la versión 1.7.0\_25 tienen problemas temporales de integración, si te aparece este mensaje al compilar/correr la app

`Could not determine Hibernate dialect for database name [H2]!`

reemplazá la versión de tu JDK por una distinta.

Integración con otros lenguajes de programación
-----------------------------------------------

-   [Integración Grails con Xtend](integracion-grails-con-xtend.html)
-   [Integración Grails con Scala](integracion-grails-con-scala.html)
-   Grails se integra naturalmente con Java, sólo hay que definir las clases Java en el source folder src/java y se incluyen con la aplicación web

