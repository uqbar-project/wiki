A continuación explicaremos como instalar un entorno de desarrollo para Java 8, el cual incluirá los siguientes elementos:

-   El JDK8: Las biblioteas estándar y herramientras de construccion para la tecnologia Java y derivadas
-   Eclipse: Un entorno integrado de desarrollo (IDE), que nos servirá para codificar, compilar, probar, refactorizar nuestro software.
-   Maven: Una herramientas de (entre otras cosas) manejo de dependencias

Nota: además de lo descripto acá, será necesario contar con una herramienta de control de versiones, como por ejemplo git.

Digresión: Por qué Java 8 y no una versión anterior?
----------------------------------------------------

El lenguaje Java 8 introduce una caracterstica fundamental en cualquier lenguaje de programación moderno: bloques de codigo (también conocidos como lambdas, closures, funciones anónimas). Trabajar sin las mismas limita mucho la expresividad de nuestro lenguaje, por lo que las versiones anteriores de Java no son las mas idoneas para plantear y validar diseños.

Instalación del JDK (Java Development Kit)
------------------------------------------

El JDK8 contiene un compilador y una máquina virtual (el runtime) que traduce a código de máquina el código intermedio que genera el compilador (.java → COMPILADOR (javac) → .class → VM (java) → ejecutable final).

Para instalarlo, debemos descargarlo de <http://www.oracle.com/technetwork/java/javase/downloads/index.html>. Luego, procederemos a descomprimirlo.

Los pasos siguientes dependen del sistema operativo. En Windows, el proceso esta guiado mayormente por el instalador. En Ubuntu, Mint y Linux similares debemos realizar lo siguiente:

-   Pararse en el directorio donde se lo descomprimió.
-   sudo mv jdk1.8.0 /usr/bin/jvm/
-   sudo update-alternatives --install /usr/bin/java java /usr/lib/jvm/jdk1.8.0/jre/bin/java 500
-   sudo update-alternatives --config java. Elegir la opcion del jdk8

Con esto, el JDK ya deberia estar instalado. Probarlo desde una terminal tipeando lo siguiente:

` java -version`

(deberia mostrar 1.8.0)

Eclipse Kepler
--------------

La instalación del eclipse es muy sencilla: hay que bajar la versión el Eclipse IDE for Java Developers que corresponda a su sistema operativo desde <http://www.eclipse.org/downloads/> y descomprimirlo en su disco rígido. Posiblemente deseen crear un acceso directo para apuntar al ejecutable. Instal

Eclipse Kepler no viene con soporte para Java 8, así que tendremos que instalarlo manualmente. Para ello, sigan los siguientes pasos: \* Con el Eclipse abierto, ir a Help -&gt; Install new software -&gt; Add Site

-   Agregar el siguiente site:

` Name: Java8Support`
` URL: `[`http://build.eclipse.org/eclipse/builds/4P/siteDir/updates/4.3-P-builds/`](http://build.eclipse.org/eclipse/builds/4P/siteDir/updates/4.3-P-builds/)

-   Seleccionar Eclipse Java 8 Support For Kepler SR2 -&gt; Eclipse JDT Tools Patch for Java 8
-   Darle Siguiente, Siguiente, Aceptar la Licencia. Y Reiniciar cuando lo pida

Maven
-----

Instalen Maven según la [Guía de Instalación de Maven](guia-de-instalacion-de-maven.html)

Links útiles
------------

-   [Amigandonos con el entorno de desarrollo](amigandonos-con-el-entorno-de-desarrollo.html)

