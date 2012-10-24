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

Svn
---

Pueden instalar el plugin de svn para eclipse basándose en [este tutorial](http://subclipse.tigris.org/servlets/ProjectProcess?pageID=p4wYuA).

Maven
-----

En Ubuntu lo más sencillo para instalar el Maven es

`sudo apt-get install maven2`

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

Plugin de Maven para Eclipse
----------------------------

Se puede instalar siguiendo las instrucciones de: <http://m2eclipse.sonatype.org/installing-m2eclipse.html>

la instalación es tan simple como utilizar el mecanismo de instalación de plugins del eclipse

m2eclipse Core Update Site: <http://m2eclipse.sonatype.org/sites/m2e>

m2eclipse Extras Update Site: <http://m2eclipse.sonatype.org/sites/m2e-extras>

Creación de un proyecto básico
------------------------------

Una vez instaladas todas las herramientas, se puede crear un proyecto en esta plataforma siguiendo [este tutorial](creacion-de-un-proyecto-maven-basico.html) (ojo, este es un tutorial básico, si necesitan usar otras tecnologías de presentación busquen los tutoriales en las páginas de las tecnologías correspondientes).
