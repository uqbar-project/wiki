Pre-requisitos
--------------

Asumimos que tenés instaladas las herramientas básicas, en particular necesitaremos que tengas una JDK, no puede ser una JRE, y recomendamos que uses 1.7 ó superior. Para más ayuda, seguí este [link](preparacion-de-un-entorno-de-desarrollo-java.html).

Framework Grails
----------------

Recomendamos descargar la última versión (*Download latest release*) del siguiente link

-   <http://grails.org/download>

y luego seguir las instrucciones que figuran en el sitio (link Installation)

Entorno integrado de desarrollo (IDE)
-------------------------------------

Si bien se pueden descargar varios plugins para eclipse, recomendamos bajarse el Spring Toolkit Suite del sistema operativo que tengan en

-   <http://www.springsource.org/downloads/sts-ggts> (**IMPORTANTE**: elegir SPRING TOOL SUITE, no GROOVY/GRAILS TOOL SUITE ya que si bien ofrece algunas facilidades para predefinir opciones, necesita buena disponibilidad de internet y no permite trabajar cómodamente con distintas versiones de compiladores Groovy)
    -   **Nota:** la versión Eclipse Juno es más liviana, las versiones más nuevas pueden resultar pesadas dependiendo de cada máquina.
    -   Más información en <http://grails.org/products/ggts>

El IDE trae consigo un Web Server Tomcat integrado con el entorno (aunque por cuestiones de performance les recomendamos que cuando prueben abran un browser que no esté embebido con el entorno de desarrollo sino que lo corran por fuera del mismo).

### Completando la instalación

-   Conviene crear un acceso directo al STS especificando a qué JDK apuntar:
    -   incluir el directorio bin en el path
    -   las comillas son importantes en caso de haber espacios en el directorio

`STS -vm "/path donde está la JDK" `

-   La primera pantalla que aparece es el Dashboard (si no aparece, navegar por la barra de herramientas hasta encontrarla), ir a la solapa Extensions. Seleccionar
    -   Grails support
    -   Groovy-Eclipse
    -   Subversive

y luego presionar el botón Install.

Una vez instalado, ir a Window &gt; Preferences y

-   configurar encoding por default en UTF-8 (como se muestra en la instalación de herramientas). Aplicar cambios
-   filtrar por la palabra "spell" y desactivar la corrección ortográfica (Spelling &gt; desactivar el check Enable spell checking). Aplicar cambios
-   filtrar por la palabra "Grails" y apuntar la versión de Grails en el directorio donde se instaló (Groovy &gt; Grails y hacer Add... Browse y buscar la carpeta correspondiente al paso 1)
-   si estás trabajando en un proxy tenés que configurar variables en el Grails Launch (TODO: Ver configuración Telecom)

Si no quieren usar Eclipse
--------------------------

Después buscar un IDE de su agrado e incorporarle el plugin para Grails, acá tienen un link para integrarlo con IDEA: <http://grails.org/IDEA+Integration>

Cómo empezar
------------

-   Tutoriales para conocer la tecnología: <http://grails.org/start>
-   Manual de referencia: <http://grails.org/doc/latest/guide/>
-   Plugins que se pueden instalar: <http://grails.org/plugins/>

