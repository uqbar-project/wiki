El objetivo de este tutorial es crear una aplicación base utilizando las siguientes tecnologías:

-   Java
-   Eclipse
-   Svn
-   Maven

Se asume la presencia de un entorno con todas esas herramientas configuradas adecuadamente.

Creación del proyecto
---------------------

### Paso 1: Creación en base a un arquetype de maven

Desde una terminal y parados sobre el workspace de eclipse ejecutar el siguiente comando maven (previo asignar valores adecuados para groupId y artifactId)

`mvn archetype:create                      \`
`    -DartifactId=basic-maven-example      \`
`    -DgroupId=com.uqbar-project.edu       \`
`    -DarchetypeArtifactId=???`

Lo importante acá es saber qué archetype quiero usar, por ejemplo: maven-archetype-webapp

### Paso 2: Agregar bibliotecas necesarias al pom

Luego de ejecutar eso tendremos el código generado en el directorio con el nombre indicado en el (en mi caso ). En ese directorio encontrarán el , si vamos a utilizar otras bibliotecas podemos agregarlas ya mismo, por ejemplo:

`       `<dependency>
`           `<groupId>`log4j`</groupId>
`           `<artifactId>`log4j`</artifactId>
`           `<version>`1.2.13`</version>
`       `</dependency>
`       `<dependency>
`           `<groupId>`commons-collections`</groupId>
`           `<artifactId>`commons-collections`</artifactId>
`           `<version>`3.1`</version>
`       `</dependency>

### Paso 3: Generación de la información para eclipse

Luego, parados en la carpeta en donde está el debemos ejecutar:

`mvn eclipse:eclipse -DdownloadSources=true -DdownloadJavadocs=true`

Esto nos creará los archivos de configuración del eclipse: y . También se ocupará de bajar de Internet todas las bibliotecas que le indicamos (por eso hay que tener conexión a Internet para ejecutar estas cosas).

**Ojo:** En este punto es importante leer los warnings que tira el maven si no puede bajar las bibliotecas, para eso hay que leer todo lo que dice, no alcanza con quedarse con el final.

### Paso 4: Importación de la información al eclipse y al tomcat

Luego, desde el eclipse se deberá importar el proyecto, utilizando la opción "Import" -&gt; "Existing projects into workspace".

Publicarlo en svn
-----------------

Al publicarlo es importante agregar al <svn:ignore> los siguientes archivos y directorios:

-   Directorio work y subdirectorios
-   Directorio target y subdirectorios.
-   Archivos .project, .classpath y .tomcatplugin

Luego quien se lo baje deberá ejecutar los pasos 2, 3, 4.2 y 4.3

Agregar más referencias posteriormente
--------------------------------------

Para agregar dependencias posteriormente se debe realizar de la siguiente manera:

-   Modificar el siguiendo las instrucciones en el paso 2.
-   Actualizar los archivos de configuración de eclipse (paso 3):

`mvn eclipse:eclipse -DdownloadSources=true -DdownloadJavadocs=true`

\*: Si tienen dependencias con otros proyectos que estén en su workspace de eclipse, una idea útil es hacer que el maven genere referencias contra esos proyectos en lugar de contra jars en su repositorio local. Para eso deben agregar a la línea anterior:

`-Dmaven.eclipse.workspace=`<path a su workspace de eclipse>

-   Refrescar el proyecto desde el eclipse (F5 sobre el proyecto o botón derecho -&gt; refresh)

Definir repositorios adicionales
--------------------------------

Estas bibliotecas no se encuentran en el repositorio default del maven (repo1.maven.com), por lo tanto debemos agregar un repositorio adicional. Hay muchas formas de hacer esto, una sencilla es agregarlo en el pom, antes de las dependencias. Un repositorio posible para esta tarea es el de JBoss, para agregarlo pueden hacer:

<repositories>
`   `<repository>
`       `<id>`jboss`</id>
`       `<url>[`http://repository.jboss.org/maven2`](http://repository.jboss.org/maven2)</url>
`   `</repository>
</repositories>

Links relacionados
------------------

-   [Algo3 Temario](algo3-temario.html)

