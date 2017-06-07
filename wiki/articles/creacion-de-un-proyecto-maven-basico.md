---
layout: article
title: Creacion de un proyecto maven basico
---

El objetivo de este tutorial es crear una aplicación base utilizando las siguientes tecnologías:

-   Java
-   Eclipse
-   Git
-   Maven

Se asume la presencia de un entorno con todas esas herramientas configuradas adecuadamente. En caso de duda recomendamos ir [al siguiente link](/wiki/articles/preparacion-de-un-entorno-de-desarrollo-java.html).

Creación del proyecto
---------------------

### Paso 1: Creación del proyecto Maven

Desde el menú principal del Eclipse seleccionamos File > New... Project... Maven Project. En la primera pantalla del asistente

* podemos elegir un [archetype](https://maven.apache.org/guides/introduction/introduction-to-archetypes.html), o bien chequear la opción "Create a simple project (skip archetype selection)" que es la opción por defecto que vamos a elegir.

Al presionar Next, nos aparece el siguiente paso, donde debemos elegir

* el **Group Id** refleja la organización para la que vamos a construir el proyecto (por lo general depende de la materia que estás cursando)

* el **Artifact Id** que se asocia al nombre del proyecto

* la versión, donde dejamos el valor por defecto

![Creación de un proyecto Maven en Eclipse](/img/maven_create_1.png)


### Paso 2: Agregar bibliotecas necesarias al pom

En el archivo pom.xml del raíz del proyecto podemos agregar bibliotecas a nuestro proyecto en el nodo dependencies...

```xml

<dependencies>
   <dependency>
       <groupId>log4j</groupId>
       <artifactId>log4j</artifactId>
       <version>1.2.13</version>
   </dependency>
   <dependency>
       <groupId>commons-collections</groupId>
       <artifactId>commons-collections</artifactId>
       <version>3.1</version>
   </dependency>
</dependencies>

```

También podemos cambiar el groupId, artifactId, la versión o bien apuntar a un [parent project](https://maven.apache.org/guides/introduction/introduction-to-the-pom.html#Project_Inheritance_vs_Project_Aggregation) (para mayor información consulte con el docente de su materia)


### Paso 3: Importación de la información al entorno

Cada vez que se modifique el archivo pom.xml, debemos actualizar nuestro entorno (Eclipse, IntelliJ o el que fuera) mediante un botón derecho sobre el proyecto Maven > Update project, o bien por línea de comando hacer:

```bash

mvn eclipse:eclipse -DdownloadSources=true -DdownloadJavadocs=true

```

Cómo encontrar bibliotecas
--------------------------

Si no estamos seguros del nombre o la última versión de un componente, podemos hacer la correspondiente búsqueda en <http://search.maven.org>


Definir repositorios adicionales
--------------------------------

Si tenemos bibliotecas que no podamos encontrar en el repositorio default de maven (repo1.maven.com), debemos agregar un repositorio adicional. Hay muchas formas de hacer esto, una sencilla es agregarlo en el pom, antes de las dependencias. Un repositorio posible para esta tarea es el de JBoss, para agregarlo pueden hacer:

```xml

<repositories>
   <repository>
      <id>jboss</id>
      <url>http://repository.jboss.org/maven2</url>
   </repository>
</repositories>

```

Links relacionados
------------------

-   Una vez creado el proyecto, para que otras personas quieran tenerlo en su propio entorno de trabajo, conviene mirar las instrucciones para [descargar un repositorio git](/wiki/articles/bajar-un-proyecto-maven-de-un-repositorio-git.html).


-   [Temario Algoritmos III](algo3-temario.html)

