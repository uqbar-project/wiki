---
layout: article
title: Preparacion de un entorno de desarrollo scala
---

# Download e instalación base

Hay varios IDEs para desarrollar con Scala, incluso podemos desarrollar sin un IDE (solamente necesitamos el compilador). De todos modos acá vamos a explicar cómo se puede armar un ambiente de desarrollo usando Eclipse y Maven (para manejo de dependencias).

- La opción más recomendada es descargar la última versión [Scala IDE](http://scala-ide.org/download/sdk.html).

-   También instalar el plugin que integra Scala con Maven del update site de [alchim31.free.fr](http://alchim31.free.fr/m2e-scala/update-site), solamente te va a proponer instalar el componente m2e-slf4j, ya que los otros vienen con la instalación default. Aceptalo y ¡ya tenés tu entorno listo! 

## Alternativa - Combinación Eclipse - Scala - Plugins

Otra opción es combinar una instalación de

- [Eclipse](http://www.eclipse.org/downloads/) 

- con el plugin del [Scala IDE for Eclipse](http://scala-ide.org/download/current.html). **Asegurate que coincida la versión del plugin con la versión de Eclipse que tengas instalado.**  

- Para ejecutar y escribir tests en ScalaTest (una especie de JUnit para Scala), tenés que instalarle un plugin opcional, que se encuentra en la misma URL que usaste para instalar el ScalaIDE. Asegurate de checkear el item bajo "Scala IDE Plugins (incubation) -&gt; ScalaTests for ScalaIDE". Pueden ver [acá](http://www.scalatest.org/user_guide/using_scalatest_with_eclipse) un screenshot.

- También instalar el plugin que integra Scala con Maven del update site de [alchim31.free.fr](http://alchim31.free.fr/m2e-scala/update-site)

Esta configuración tiene un bug que impide la utilización del visualizador de POMs que viene con el plugin de Maven. Un *workaround* para el problema del POM es abrirlo con el editor de XML.

Por favor si notás que esta información está desactualizada reportalo.

# Creación de un proyecto Maven con Scala

La configuración de un proyecto Scala para poder utilizar Maven es relativamente compleja y tiene varias sutilezas, principalmente para poder integrar ambas herramientas dentro del Eclipse. Por eso, recomendamos la utilización de este parent project que creamos con este objetivo específico:

```xml
   <parent>
       <groupId>org.uqbar-project</groupId>
       <artifactId>uqbar-scala-parent</artifactId>
       <version>1.3</version>
   </parent>
```

Para poder utilizar ese parent project necesario realizar previamente realizar las tareas indicadas en [Configuración de Maven para poder utilizar las herramientas de Uqbar](configuracion-de-maven-para-poder-utilizar-las-herramientas-de-uqbar.html)

Este parent project realiza varias configuraciones:

- Agrega las dependencias con Scala y ScalaTest
- Define src/main/scala como directorio default donde están los fuentes (y src/test/scala donde están los tests).
- Le indica al maven que utilice el compilador de Scala.
- Configura la integración con el Eclipse.

# Documentación

- [Documentación oficial](http://www.scala-lang.org/node/197)
- [Manuales de referencia](http://www.scala-lang.org/node/198)
- [Scala by Example](http://www.scala-lang.org/docu/files/ScalaByExample.pdf#) escrito por Martin Odersky, el creador de Scala.
- [Effective Scala](http://twitter.github.io/effectivescala/)

# Errores posibles

Si te aparece un mensaje de error

```bash
scalatest_2.9.1-1.6.1.jar is cross-compiled with an incompatible version of Scala (2.9.1). 
In case of errorneous report, this check can be disabled in the compiler preference page.
```

la solución es Window &gt; Preferences &gt; Scala Compiler &gt; solapa Build Manager y deschequear la opción withVersionClasspathValidator. Hacer un rebuild de todos los proyectos.

# Links de interés

- [Página principal de Scala](http://www.scala-lang.org/)
- [Un tutorial de Scala](http://paco.uqbar-project.org/te/scala/introduccin-a-scala) de Javi Fernándes, miembro de Uqbar
