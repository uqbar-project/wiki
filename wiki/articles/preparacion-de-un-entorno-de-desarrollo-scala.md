Download e instalación base
---------------------------

Hay varios IDEs para desarrollar con Scala, incluso podemos desarrollar sin un IDE (solamente necesitamos el compilador). De todos modos acá vamos a explicar cómo se puede armar un ambiente de desarrollo usando Eclipse y Maven (para manejo de dependencias).

Lo podemos usar directamente como un plugin desde el eclipse

-   Bajar el [Eclipse](http://www.eclipse.org/downloads/) más te guste. Recomendamos Eclipse IDE for Java EE Developers. Fijate de elegir uno que se adapte al sistema operativo de tu PC y que sea de 32 o 64 bits según corresponda (esta es la única herramienta dependiente de la plataforma, las demás deberían servir tanto para diferentes sistemas operativos como para diferentes arquitecturas de procesador).
-   Instalale el plugin del [Scala IDE for Eclipse](http://scala-ide.org/download/current.html). Asegurate que coincida la versión del plugin con la versión de Eclipse que tengas instalado.
    -   Instalale el plugin de Maven ([m2eclipse](http://www.sonatype.org/m2eclipse)). Si elegiste el Eclipse for Java Developers ya viene con este plugin.
    -   Instalale el plugin para integrar scala con Maven ([m2eclipse-scala](https://github.com/sonatype/m2eclipse-scala)).
    -   Instalar uno o más plugins para los repositorios de código que uses (svn, mercurial, git). Si elegiste el Eclipse for Java Developers ya viene con el plugin de git.

### Versiones

Al día 13 de agosto de 2013, una configuración posible es:

-   Eclipse Kepler (v4.3) for Java Developers (incluye plugins para maven y git)
-   [Scala IDE 3.0.1](http://download.scala-ide.org/sdk/e38/scala210/stable/site) (para Scala 2.10 y Eclipse 3.8 o posterior).
-   m2eclipse-scala del update site de [alchim31.free.fr](http://alchim31.free.fr/m2e-scala/update-site)

Esta configuración tiene un bug que impide la utilización del visualizador de POMs que viene con el plugin de Maven. Un *workaround* para el problema del POM es abrirlo con el editor de XML.

Aún así, recomendamos esa configuración. Otras configuraciones posibles son:

-   Utilizar [Scala IDE 4.0.0 (milestone para Scala 2.11-M4)](http://scala-ide.org/download/milestone.html). Esta versión no tiene el problema del POM pero presenta otros inconvenientes:
    -   Tanto el Scala 2.11 como el Scala IDE 4.0.0 son versiones *milestone* es decir, sus propios desarrolladores aún no las consideran versiones finales.
    -   Algunas librerías necesarias para Testear no están disponibles para Scala 2.11
-   Utilizar Eclipse Indigo (v3.7).

Por favor si notás que esta información está desactualizada reportalo.

Creación de un proyecto Maven con Scala
---------------------------------------

La configuración de un proyecto Scala para poder utilizar Maven es relativamente compleja y tiene varias sutilezas, principalmente para poder integrar ambas herramientas dentro del Eclipse. Por eso, recomendamos la utilización de este parent project que creamos con este objetivo específico:

`   `<parent>
`       `<groupId>`org.uqbar-project`</groupId>
`       `<artifactId>`uqbar-scala-parent`</artifactId>
`       `<version>`1.0`</version>
`   `</parent>

Para poder utilizar ese parent project necesario realizar previamente realizar las tareas indicadas en [Configuración de Maven para poder utilizar las herramientas de Uqbar](configuracion-de-maven-para-poder-utilizar-las-herramientas-de-uqbar.html)

Este parent project realiza varias configuraciones:

-   Agrega las dependencias con Scala y ScalaTest
-   Define src/main/scala como directorio default donde están los fuentes (y src/test/scala donde están los tests).
-   Le indica al maven que utilice el compilador de Scala.
-   Configura la integración con el Eclipse.

Documentación
-------------

-   [Documentación oficial](http://www.scala-lang.org/node/197)
-   [Manuales de referencia](http://www.scala-lang.org/node/198)
-   [Scala by Example](http://www.scala-lang.org/docu/files/ScalaByExample.pdf#) escrito por Martin Odersky, el creador de Scala.
-   [Effective Scala](http://twitter.github.io/effectivescala/)

Errores posibles
----------------

Si te aparece un mensaje de error

`scalatest_2.9.1-1.6.1.jar is cross-compiled with an incompatible version of Scala (2.9.1). In case of errorneous report, this check can be disabled in the compiler preference page.`

la solución es botón derecho sobre el proyecto &gt; Properties &gt; Scala Compiler &gt; solapa Build Manager y deschequear la opción withVersionClasspathValidator.

Links de interés
----------------

-   [Página principal de Scala](http://www.scala-lang.org/)
-   [Groovy vs Scala](groovy-vs-scala.html)

