---
layout: article
title: Creacion de un proyecto web basado en jsp y servlets
featured: true
---

El presente tutorial quedó deprecado, consulte con su docente por un tutorial más actualizado.

# Introducción

El objetivo de este tutorial es crear una aplicación base utilizando las siguientes tecnologías:

-   Java
-   Eclipse
-   Tomcat
-   Sysdeo + DevLoader + Tomcat-XT
-   Maven

Se asume la presencia de un entorno con todas esas herramientas configuradas adecuadamente.

# Creación del proyecto

## Paso 1: Creación en base a un arquetype de maven

Desde una terminal y parados sobre el workspace de eclipse ejecutar el siguiente comando maven (previo asignar valores adecuados para groupId y artifactId)

```bash
mvn archetype:create                             \
    -DartifactId=basic-jsp-example               \
    -DgroupId=com.uqbar-project.edu.progui       \
    -DarchetypeArtifactId=maven-archetype-webapp 
```

## Paso 2: Agregar bibliotecas necesarias al pom

Luego de ejecutar eso tendremos el código generado en el directorio con el nombre indicado en el (en mi caso ). En ese directorio encontrarán el , si vamos a utilizar servlets (que es lo más probable) hay que editarlo para agregar las bibliotecas correspondientes:

```xml
       <dependency>
           <groupId>javax.servlet</groupId>
           <artifactId>servlet-api</artifactId>
           <version>2.5</version>
           <scope>provided</scope>
       </dependency>
       <dependency>
           <groupId>javax.servlet.jsp</groupId>
           <artifactId>jsp-api</artifactId>
           <version>2.1</version>
           <scope>provided</scope>
       </dependency>
```

## Paso 3: Crear una carpeta adicional para las clases java

Dentro del mismo directorio veremos una carpeta y dentro dos carpetas con fuentes


-   `src/main/webapp`: es el lugar donde se ubicarán nuestras páginas web, y dendro de ella en la carpeta  se encuentra la configuración del tomcat.
-   `src/main/resources`: que estará vacía y es el lugar donde poner archivos de configuración y otros recursos *no web* (toda la información web va en webapp).  

Es necesario crear una carpeta más:

-   `src/main/java`: es el lugar para ubicar nuestras clases Java.  

<!-- -->

## Paso 4: Generación de la información para eclipse

Luego, parados en la carpeta en donde está el debemos ejecutar:

```bash
mvn eclipse:eclipse -DdownloadSources=true -DdownloadJavadocs=true
```

Esto nos creará los archivos de configuración del eclipse. También habrá bajado todas las bibliotecas que le indicamos. En este punto es importante leer los warnings que tira el maven si no puede bajar las bibliotecas, para eso hay que leer todo lo que dice, no alcanza con quedarse con el final.

## Paso 5: Generación de la información para sysdeo

El último paso será generar la información necesaria para el plugin de sysdeo.

```bash
mvn sysdeo-tomcat:generate
```

## Paso 6: Importación de la información al eclipse y al tomcat

Luego, desde el eclipse se deberá:

1.  Importarlo al eclipse, utilizando la opción "Import" -&gt; "Existing projects into workspace".
2.  Agregar el proyecto al contexto del tomcat. Para ello: botón derecho sobre el proyecto -&gt; "Tomcat project" -&gt; "Update context definition"
3.  Levantar el tomcat utilizando el plugin de sysdeo.

<!-- -->

Una vez finalizado todo esto se puede ingresar desde su explorador favorito y ver que todo funciona, en mi caso la URL de prueba es:

```html
http://localhost:8080/example.jsp/
```

# Publicarlo en svn

Al publicarlo es importante agregar al <svn:ignore> los siguientes archivos y directorios:

-   Directorio work y subdirectorios
-   Directorio target y subdirectorios.
-   Archivos .project, .classpath y .tomcatplugin

Luego quien se lo baje deberá ejecutar los pasos 2, 3, 4.2 y 4.3

<!-- -->

# Cómo agregar bibliotecas adicionales

Para poder tener soporte para EL y JSTL en el proyecto eclipse, se deben agregar las siguientes dependencias en el pom y actualizar el proyecto

## Agregar la dependencia en el pom

Expression Languaje  

```xml
<dependency>
   <groupId>javax.el</groupId> 
   <artifactId>el-api</artifactId>
   <version>2.2</version>
   <optional>true</optional>
</dependency>
```

JSTL  

```xml
<dependency>
   <groupId>javax.servlet</groupId>
   <artifactId>jstl</artifactId>
   <version>1.2</version>
   <scope>runtime</scope>
</dependency>
```

## Definir repositorios adicionales

Estas bibliotecas no se encuentran en el repositorio default del maven (repo1.maven.com), por lo tanto debemos agregar un repositorio adicional. Hay muchas formas de hacer esto, una sencilla es agregarlo en el pom, antes de las dependencias. Un repositorio posible para esta tarea es el de JBoss, para agregarlo pueden hacer:

```xml
<repositories>
   <repository>
       <id>jboss</id>
       <url>[http://repository.jboss.org/maven2](http://repository.jboss.org/maven2)</url>
   </repository>
</repositories>
```

## Actualizar el proyecto eclipse

Una vez hecho esto deben:

-   Actualizar los archivos de configuración de eclipse y sysdeo:

```bash
mvn eclipse:eclipse sysdeo-tomcat:generate -DdownloadSources=true -DdownloadJavadocs=true
```

-   Si tienen dependencias con otros proyectos que estén en su workspace de eclipse, una idea útil es hacer que el maven genere referencias contra esos proyectos en lugar de contra jars en su repositorio local. Para eso deben agregar a la línea anterior:

```bash
-Dmaven.eclipse.workspace=<path a su workspace de eclipse>
```

{% wiki_note_alert %}
Ojo: Eso funciona para el eclipse pero no para el sysdeo así que en caso de utilizar esta idea deberán modificar el path del devloader a mano (no todavía, eso se puede hacer después del siguiente paso)
{% endwiki_note_alert %}

-   Refrescar el proyecto desde el eclipse (F5 sobre el proyecto o botón derecho -&gt; refresh)

Ahora sí estaríamos en condiciones de modificar el path del devloader a mano. Para eso ir a propiedades del proyecto -&gt; Tomcat -&gt; DevLoader classpath. Ahí deberían ver que las referencias a sus proyectos no están selecionadas, deben agregarlas antes de ir al siguiente paso.

Esto es manual y feo, estamos trabajando en una propuesta más prolija que no implique una enorme cantidad adicional de burocracia, mientras tanto es lo mejor que tenemos.

-   Actualizar el proyecto en el sysdeo (botón derecho sobre el proyecto -&gt; Tomcat Project -&gt; Update Context Definition

## Otras configuraciones necesarias

Para poder usar JSTL además del jar (que bajó el maven en el apartado anterior), se necesitan un par de cosas más:

Descripción del tag library  

En un archivo , por ejemplo el . Todos los archivos tld estándar ya los bajó el maven, pero hay que copiarlos a la carpeta WEB-INF, es decir:

-   Buscar el jar de jstl que debería estar en
-   Dentro del jar hay una carpeta META-INF y están todos los tlds que necesitamos.
-   Copiar el a la carpeta de nuestro proyecto web (es decir

Adicionalmente es necesario indicar en el qué tag libraries queremos utilizar, por ejemplo para agregar el debemos indicar:

```xml
<taglib>
   <taglib-uri>[http://java.sun.com/jstl/core](http://java.sun.com/jstl/core)</taglib-uri>
   <taglib-location>c.tld</taglib-location>
</taglib>
```

## Indicaciones en la página   

Para usar EL:

```
<%@ page isELIgnored ="false" %> 
```

Para usar JSTL:

```
<%@ page pageEncoding="UTF-8" %> 
<%@ taglib uri="[http://java.sun.com/jstl/core](http://java.sun.com/jstl/core)" prefix="c" %>
```

# Links relacionados

-   [Temario Algoritmos III](algo3-temario.html)

