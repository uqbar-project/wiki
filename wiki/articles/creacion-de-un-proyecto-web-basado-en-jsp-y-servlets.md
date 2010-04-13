El objetivo de este tutorial es crear una aplicación base utilizando las siguientes tecnologías:

-   Java
-   Eclipse
-   Tomcat
-   Sysdeo + DevLoader +
-   Maven

Se asume la presencia de un entorno con todas esas herramientas configuradas adecuadamente.

Creación del proyecto
---------------------

Paso 1  

`mvn archetype:create `
`    -DgroupId=com.uqbar-project.edu.progui  `
`    -DartifactId=basic-example.jsp`
`    -DarchetypeArtifactId=maven-archetype-webapp`

Veremos que nos creó un directorio con el nombre indicado por , si miramos dentro de ese directorio encontraremos el del proyecto y dos carpetas de fuentes:

src/main/java:Es el lugar para ubicar nuestras clases Java.
src/main/webapp:Es el lugar donde se ubicarán nuestras páginas web, y dendro de ella en la carpeta  se encuentra la configuración del tomcat.  

<!-- -->

Paso 2  

Luego, ejecutando:

`mvn eclipse:eclipse -DdownloadSources=true -DdownloadJavadocs=true`

Paso 3  

Eso deja preparado el proyecto para importarlo al eclipse, y el último paso será generar la información necesaria para el plugin de sysdeo.

`mvn sysdeo-tomcat:generate`

Paso 4  

Luego, desde el eclipse se deberá:

1.  Importarlo al eclipse, utilizando la opción "Import" -&gt; "Existing projects into workspace".
2.  Agregar el proyecto al contexto del tomcat. Para ello: botón derecho sobre el proyecto -&gt; "Tomcat project" -&gt; "Update context definition"
3.  Levantar el tomcat utilizando el plugin de sysdeo.

Una vez finalizado todo esto se puede ingresar desde su explorador favorito y ver que todo funciona, en mi caso la URL de prueba es:

[`http://localhost:8080/example.jsp/`](http://localhost:8080/example.jsp/)

Publicarlo en svn
-----------------

Al publicarlo es importante agregar al <svn:ignore> los siguientes archivos y directorios:

-   Directorio work y subdirectorios
-   Directorio target y subdirectorios.
-   Archivos .project, .classpath y .tomcatplugin

Luego quien se lo baje deberá ejecutar los pasos 2, 3, 4.2 y 4.3

Bibliotecas adicionales
-----------------------

Para poder usar Expresssion Language dentro de las páginas JSP, se debe

1.  Incluir esta dependencia en el pom.

<dependency>
`   `<groupId>`org.glassfish.web`</groupId>
`   `<artifactId>`jstl-impl`</artifactId>
`   `<version>`1.2`</version>
</dependency>

1.  Actualizar los archivos de configuración de eclipse y sysdeo:

`mvn eclipse:eclipse sysdeo-tomcat:generate -DdownloadSources=true -DdownloadJavadocs=true`

1.  Refrescar el proyecto desde el eclipse (F5 sobre el proyecto o botón derecho -&gt; refresh)
2.  Actualizar el proyecto en el sysdeo (botón derecho sobre el proyecto -&gt; Tomcat Project -&gt; Update Context Definition

