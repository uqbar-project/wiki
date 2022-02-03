---
layout: article
title: Preparacion de un entorno de desarrollo scala
---

# Download e instalación base

Hay varios IDEs para desarrollar con Scala, incluso podemos desarrollar sin un IDE (solamente necesitamos el compilador). De todos modos acá vamos a explicar cómo se puede armar un ambiente de desarrollo usando Eclipse y Maven (para manejo de dependencias).

- La opción más recomendada es trabajar con [IntelliJ](https://www.jetbrains.com/help/idea/discover-intellij-idea-for-scala.html)

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
- [Scala by Example](https://www.scala-lang.org/old/sites/default/files/linuxsoft_archives/docu/files/ScalaByExample.pdf) escrito por Martin Odersky, el creador de Scala.
- [Effective Scala](http://twitter.github.io/effectivescala/)

# Links de interés

- [Página principal de Scala](https://www.scala-lang.org/)
- [Un tutorial de Scala](http://paco.uqbar-project.org/te/scala/introduccin-a-scala) de Javi Fernándes, miembro de Uqbar
