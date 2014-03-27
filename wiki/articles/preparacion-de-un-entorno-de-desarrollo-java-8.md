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

Instalar Maven Ir a maven.apache.org/download.cgi Buscar la última versión de Maven (3.2.1 a la fecha). Descargar y descomprimir Agregar al path. Crear un proyecto de prueba con maven mvn archetype:create -DgroupId=ar.edu.utn.frba.dds.demo -DartifactId=demo Configurar el compilador para que utilice Java 8. Ver <http://maven.apache.org/plugins/maven-compiler-plugin/examples/set-compiler-source-and-target.html> Configurar el WS Desactivar descar de indices de Maven Window -&gt; Preferences -&gt; Maven -&gt;

`   Marcar Offline`
`   Desmarcar Download repository indexes`

Importar el proyecto de prueba en eclipse

Maven
-----

### Instalación default

Pueden seguir [estas instrucciones](http://maven.apache.org/download.cgi#Installation)

### Instalación en SO Unix-based (Linux, Solaris and Mac OS X)

Descargar Apache Maven 3 desde [este link](http://apache.dattatec.com/maven/maven-3/3.0.4/binaries/apache-maven-3.0.4-bin.tar.gz).

Descomprimir el tarball y mover el directorio a donde usualmente se guardan los programas. Ejemplo: */home/john/programs/*.

`$ tar -xzvf apache-maven-3.0.4-bin.tar.gz`
`$ mv apache-maven-3.0.4 /home/john/programs/`

Agregar la siguiente línea al archivo **.bashrc**. Este archivo oculto (su nombre empieza con '.') contiene comandos que se ejecutan cuando se abre una terminal (consola). Se puede abrir con cualquier editor de textos (Gedit, vim, Emacs, Notepad++, etc) y se encuentra en el directorio **home** del usuario.

`export PATH=$PATH:$HOME/programs/apache-maven-3.0.4/bin`

Una forma sencilla de hacer ésto (sin tener que abrir un editor) es usando el programa **echo** y *appendeando* (redireccionando y agregando al final) el output al archivo. **Prestar atención al hecho de que se usan dos signos mayor**:

`$ echo 'export PATH=$PATH:$HOME/programs/apache-maven-3.0.4/bin' >> .bashrc`

Corroboramos que podemos usar Maven. El output sería algo parecido a ésto:

`john@notebook:~$ mvn -v`
`Apache Maven 3.0.4 (r1232337; 2012-01-17 05:44:56-0300)`
`Maven home: /home/john/programs/apache-maven-3.0.4`
`Java version: 1.6.0_26, vendor: Sun Microsystems Inc.`
`Java home: /usr/lib/jvm/java-6-sun-1.6.0.26/jre`
`Default locale: en_US, platform encoding: UTF-8`
`OS name: "linux", version: "3.0.0-19-generic", arch: "i386", family: "unix"`

### Configuración de Maven

La configuración general de Maven se encuentra en el archivo . Hay varios lugares donde se puede ubicar este archivo de configuración, habitualmente utilizaremos el que se encuentra en el directorio del usuario, en la ruta . La ruta exacta dependiendo de cada sistema operativo. Por ejemplo en Linux, si el usuario es pablo, la ruta sería . Si trabajan con Windows, instalado en el drive C: y el nombre del usuario es Fernando, el directorio será .

Es posible que la primera vez que agregamos una configuración, este archivo no exista. En ese caso se debe crearlo.

### Propiedades de Maven en Eclipse

Window &gt; Preferences te permite configurar algunas propiedades para Maven. Te recomendamos

-   tener chequeado "Do not automatically update dependencies from remote repositories" para que no intente bajarte permanentemente nuevas versiones de los componentes que utilices. Esto requiere que lo hagas en forma manual, algo que quizás sea más recomendable.
-   tener chequeado "Download artifact sources" te permite ver el código fuente de los .jars que te bajes, esta opción hace que las descargas inicialmente tarden un poco más de tiempo pero es bueno cuando tenés algún error y necesitás entender cómo funciona alguna parte de un componente.
-   también es bueno chequear "Download artifact javadocs" para obtener documentación de los componentes que utilizamos
-   Y por último tener deschequeada la opción "Update Maven projects on startup" permite que manualmente vos actualices los proyectos solamente ante un cambio y no cuando levantes el Eclipse.

Una configuración más que puede ser útil para encontrar versiones nuevas de artefactos en los repositorios es dejar chequeada:

-   La opción "Download repository index on startup" (opción por defecto chequeada): para más información pueden leer <http://stackoverflow.com/questions/8647769/what-is-eclipse-doing-when-it-says-that-its-updating-indexes>.

### Creación de un proyecto básico

Una vez instaladas todas las herramientas, se puede crear un proyecto en esta plataforma siguiendo [este tutorial](creacion-de-un-proyecto-maven-basico.html) (ojo, este es un tutorial básico, si necesitan usar otras tecnologías de presentación busquen los tutoriales en las páginas de las tecnologías correspondientes).

#### A manopla

-   Creamos un proyecto Maven.

`$ mvn archetype:generate -DarchetypeGroupId=org.apache.maven.archetypes -DgroupId=org.uqbar.arena.examples -DartifactId=Example`

A todas las opciones que nos pregunte le damos enter para aceptar las default.

-   Copiamos nuestro código al directorio generado para el groupId. En el ejemplo: src/main/java/ar/edu/frba/utn/dds.

<!-- -->

-   Probamos hacer una compilación e instalación local.

`$ mvn install`

Links útiles
------------

-   [Amigandonos con el entorno de desarrollo](amigandonos-con-el-entorno-de-desarrollo.html)

