El objetivo de este tutorial es crear una aplicación base utilizando las siguientes tecnologías:

-   Java
-   Eclipse
-   Tomcat
-   Sysdeo + DevLoader +
-   Maven

Se asume la presencia de un entorno con todas esas herramientas configuradas adecuadamente.

Creación del proyecto
---------------------

`mvn archetype:create `
`    -DgroupId=basic-example.jsp `
`    -DartifactId=com.uqbar-project.edu.progui `
`    -DarchetypeArtifactId=maven-archetype-webapp`

Veremos que nos creó un directorio con el nombre indicado por , si miramos dentro de ese directorio encontraremos el del proyecto y dos carpetas de fuentes:

src/main/java:Es el lugar para ubicar nuestras clases Java.
src/main/webapp:Es el lugar donde se ubicarán nuestras páginas web, y dendro de ella en la carpeta  se encuentra la configuración del tomcat.  

Luego, ejecutando:

`mvn eclipse:eclipse -DdownloadSources=true -DdownloadJavadocs=true`

Eso deja preparado el proyecto para importarlo al eclipse, y el último paso será generar la información necesaria para el plugin de sysdeo.

`mvn sysdeo-tomcat:generate`

Luego, desde el eclipse se deberá:

-   Importarlo al eclipse, utilizando la opción "Import" -&gt; "Existing projects into workspace".
-   Agregar el proyecto al contexto del tomcat. Para ello: botón derecho sobre el proyecto -&gt; "Tomcat project" -&gt; "Update context definition"
-   Levantar el tomcat utilizando el plugin de sysdeo.

Una vez finalizado todo esto se puede ingresar desde su explorador favorito y ver que todo funciona, en mi caso la URL de prueba es:

[`http://localhost:8080/example.jsp/`](http://localhost:8080/example.jsp/)
