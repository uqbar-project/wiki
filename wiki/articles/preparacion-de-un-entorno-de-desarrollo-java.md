Para realizar aplicaciones de complejidad mediana-grande en Java, es recomendable contar con un entorno de trabajo que contemple al menos:

-   Una herramienta de versionado de fuentes
-   Una herramienta de manejo de dependencias
-   Un mecanismo para automatizar los procesos administrativos del desarrollo (test, release, deploy, etc)
-   Un entorno de programación que permita:
    -   Ayudas a la detección temprana de errores, autocompleción, herramientas para navegar y buscar ágilmente dentro del código, etc.
    -   Soporte para la realización de refactors automatizados
    -   Integración con la mayor cantidad posible de las demás herramientas que utilizamos.

En este artículo se propone una configuración de entorno de trabajo que intenta cumplir con los anteriores objetivos. Las herramientas seleccionadas para eso son:

-   **Java Development Kit**
-   **Eclipse** como entorno integrado de desarrollo
-   **Svn** como repositorio de fuentes y herramienta de versionado
-   **Maven** como herramienta para manejar dependencias y automatizar diversos procesos administrativos.

Adicionalmente se instalarán extensiones al entorno de desarrollo eclipse para integrarlo con svn y maven.

JDK (Java Development Kit)
--------------------------

Contiene un compilador y una máquina virtual (el runtime) que traduce a código de máquina el código intermedio que genera el compilador (.java → COMPILADOR (javac.exe) → .class → VM (java.exe) → ejecutable final).

Al tiempo de escribir este artículo la última versión estable es 1.6. Para el propósito aquí descripto es recomendable instalar la *Standard Edition*.

### Download e instalación base

A continuación se detallan los pasos básicos de instalación según el sistema operativo que se esté utilizando. Luego de realizar este paso inicial se deberá pasar a la configuración del entorno.

Este es el link de [downloads](http://www.oracle.com/technetwork/java/javase/downloads/index.html).

Desde ahí buscan el Latest Release (a la fecha de hoy es Java Platform (JDK) 7u7) y se descargan el JDK del sistema operativo que esté instalado en sus máquinas.

Para más detalles adicionales a los que se encuentran en esta página, se puede consultar el [manual de instalación de sun](http://java.sun.com/javase/6/webnotes/install/index.html).

#### Ubuntu

Para instalarlo en Ubuntu se puede hacer:

`sudo apt-get install default-jdk`

Eso instalará el jdk default, que actualmente en Ubuntu es *OpenJDK 6*, si se desea instalar en cambio el JDK de Sun se puede hacer:

`sudo apt-get install sun-java6-jdk sun-java6-jre`

#### Otros sistemas operativos

Para otros sistemas operativos se puede bajar el instalable de: <http://java.sun.com/javase/downloads/> .

[Este tutorial](http://www.it.uc3m.es/tlp/guia/guiaWinXP.html) indica cómo instalarlo en Windows XP (en especial pasos 1 a 3).

Eclipse
-------

La instalación del eclipse es muy sencilla: hay que bajar el que corresponda a su sistema operativo desde <http://www.eclipse.org/downloads/> y descomprimirlo en su disco rígido. Posiblemente deseen crear un acceso directo para apuntar al ejecutable. A los efectos de los objetivos planteados en este artículo, se recomienda elegir la versión denominada "Eclipse IDE for Java EE Developers".

  
Esa versión pesa bastante. Si no van a utilizar las herramientas de programación web es posible utilizar la versión más liviana "Eclipse IDE for Java Developers".

### Configuraciones adicionales

Para no tener problemas con los tildes y demás caracteres especiales al bajarse los ejemplos conviene tener sincronizado el mismo encoding. Para eso, desde la barra de menú: Window &gt; Preferences, filtrar por "encoding" y cambiar todos a "UTF-8" o "ISO 10646/Unicode(UTF-8)". Por ejemplo: En General &gt; Workspace &gt; Text file encoding, seleccionar Other &gt; UTF-8.

También conviene desactivar el warning default de clases serializables que no definan un identificador de versión: Window &gt; Preferences, filtrar por "Serializable", solapa Java / Compiler / "Errors/Warnings", "Potential programming problems", y se setea el valor de "Serializable class without serialVersionUID" a Ignore.

Svn
---

Pueden instalar el plugin de svn para eclipse basándose en [este tutorial](http://subclipse.tigris.org/servlets/ProjectProcess?pageID=p4wYuA).

Maven
-----

### Instalación

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

### ¡Usé un sólo '&gt;' y se me borró todo el .bashrc! ¿Ahora qué a hago?

1. Don't panic

2. Cuando editamos los archivos, Linux guarda una copia oculta del estado anterior del mismo. Simplemente, restauramos esa versión:

`$ cp .bashrc~ .bashrc`

En otros sistemas operativos o para configuraciones más específicas se pueden seguir las instrucciones de: <http://tadp.wikidot.com/maven>

A la configuración original de Maven, hay que indicarle cuales son los repositorios de donde bajar los artefactos de Arena. Para poder realizar esto es necesario crear el archivo ~/.m2/settings.xml. Este archivo se encuentra en el directorio del usuario, esta ruta cambia dependiendo de cada sistema operativo. Por ejemplo en Linux, si el usuario es pablo, la ruta sería /home/pablo/.m2/settings.xml. Si trabajan con Windows, instalado en el drive C: y el nombre del usuario es Fernando, el directorio será C:\\Users\\Fernando\\.m2\\settings.xml

El contenido de este archivo debería quedar así:

` `<settings xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
        http://maven.apache.org/xsd/settings-1.0.0.xsd">
`  `<profiles>
`        `<profile>
`              `<id>`uqbar-wiki`</id>
`              `<repositories>
`                    `<repository>
`                        `<id>`uqbar-wiki.org-releases`</id>
`                        `<name>`uqbar-wiki.org-releases`</name>
`                        `<url>[`http://uqbar-wiki.org/mvn/releases`](http://uqbar-wiki.org/mvn/releases)</url>
`                    `</repository>
`                    `<repository>
`                        `<snapshots/>
`                        `<id>`uqbar-wiki.org-snapshots`</id>
`                        `<name>`uqbar-wiki.org-snapshots`</name>
`                        `<url>[`http://uqbar-wiki.org/mvn/snapshots`](http://uqbar-wiki.org/mvn/snapshots)</url>
`                   `</repository>
`              `</repositories>
`        `</profile>
`  `</profiles>
`  `<activeProfiles>
`       `<activeProfile>`uqbar-wiki`</activeProfile>
`  `</activeProfiles>
` `</settings>

### Plugin para Eclipse

Desde la versión 4.2 (Juno) Eclipse incluyó el plugin m2e como parte de la instalación. Por lo cual no hace falta instalarlo.

Si por algún motivo estuvieras usando una versión anterior, se puede instalar de la siguiente manera:

- *Help* -&gt; *Install new software..*

- *Add..*

- En *Location* pegar la siguiente url: <http://download.eclipse.org/technology/m2e/releases> y click en *Ok*

- Se cargará el paquete **Maven Integration for Eclipse** en el recuadro blanco del centro. Seleccionarlo y hacer click en Next -&gt; Next -&gt; ... -&gt; Finish

### Creación de un proyecto básico

Una vez instaladas todas las herramientas, se puede crear un proyecto en esta plataforma siguiendo [este tutorial](creacion-de-un-proyecto-maven-basico.html) (ojo, este es un tutorial básico, si necesitan usar otras tecnologías de presentación busquen los tutoriales en las páginas de las tecnologías correspondientes).
