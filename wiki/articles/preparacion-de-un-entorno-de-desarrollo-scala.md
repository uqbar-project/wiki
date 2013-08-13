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

Documentación
-------------

-   [Documentación oficial](http://www.scala-lang.org/node/197)
-   [Manuales de referencia](http://www.scala-lang.org/node/198)
-   [Scala by Example](http://www.scala-lang.org/docu/files/ScalaByExample.pdf#) escrito por Martin Odersky, el creador de Scala.

Links de interés
----------------

-   [Página principal de Scala](http://www.scala-lang.org/)
-   [Groovy vs Scala](groovy-vs-scala.html)

