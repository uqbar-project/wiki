Pre-requisitos
--------------

Asumimos que tenés instaladas las herramientas básicas, en particular necesitaremos que tengas una JDK (no sirve que sea JRE).

Para más ayuda, seguí este [link](preparacion-de-un-entorno-de-desarrollo-java.html).

Entorno integrado de desarrollo (IDE)
-------------------------------------

Si bien se pueden descargar varios plugins para eclipse, recomendamos bajarse el Groovy/Grails Toolkit Suite del sistema operativo que tengan en

-   <http://www.springsource.org/downloads/sts-ggts> (**IMPORTANTE**: elegir GROOVY/GRAILS TOOL SUITE, no SPRING TOOL SUITE)
    -   **Nota:** para máquinas con algunos años es posible que funcione mejor con la versión Eclipse Juno. En caso contrario podés probar versiones más nuevas.
    -   Más información en <http://grails.org/products/ggts>

El IDE trae consigo

-   el framework Grails (fijo, apuntando al último release)
-   el compilador de Groovy que forma el par Grails-Groovy asociado
-   un Web Server Tomcat integrado con el entorno (aunque por cuestiones de performance les recomendamos que cuando prueben abran un browser que no esté embebido con el entorno de desarrollo sino que lo corran por fuera del mismo).

### Si quieren instalarlo manualmente

También pueden instalar el STS (Spring Toolkit Suite) y luego incorporarle el Grails support | Groovy compiler. Esto les permitirá jugar con el framework Grails y el compilador Groovy que uds. quieran.

### Si no quieren usar Eclipse

Recomendamos descargar la última versión (*Download latest release*) del siguiente link

-   <http://grails.org/download>

y luego seguir las instrucciones que figuran en el sitio (link Installation)

Después buscar un IDE de su agrado e incorporarle el plugin para Grails, acá tienen un link para integrarlo con IDEA: <http://grails.org/IDEA+Integration>

Cómo empezar
------------

-   Tutoriales para conocer la tecnología: <http://grails.org/start>
-   Manual de referencia: <http://grails.org/doc/latest/guide/>
-   Plugins que se pueden instalar: <http://grails.org/plugins/>

