---
layout: article
title: JDK vs. JRE
---

![jdk set of tools](/img/wiki/JDKandJREandJVM.png)

Cuando nos piden "instalar Java" (a secas) en nuestra máquina, es importante tener en claro la diferencia entre:

- **JRE** o _Java Runtime Environment_: un conjunto de herramientas que permite ejecutar código compilado en Java. El _environment_ o ambiente donde viven los objetos se implementa con la JVM (_Java Virtual Machine_).
- **JDK** o _Java Development Kit_: incluye al JRE y además le agrega herramientas propias para desarrollar en Java: el compilador (`javac`), el generador de documentación html para las clases (`javadoc`), el debugger (`jdb`), entre otros.

Para tener el listado completo de las herramientas que trae OpenJDK (una de las tantas variantes), recomendamos ingresar a [esta página](https://openjdk.java.net/tools/index.html).

## Arquitectura general del entorno Java

En este diagrama vemos el proceso de desarrollo de un programa Java desde que lo escribimos hasta que se ejecuta en una máquina (nuestra o de un usuario). Para compilar el programa Java a un código intermedio o _bytecode_ necesitamos el ejecutable `javac`, que viene con el JDK. El archivo `.class` generado puede ser interpretado en cualquier otro sistema operativo, solo necesitamos tener el JRE adecuado. Ejecutamos entonces el programa `java` (o `javaw` en Windows) pasando como argumento nuestro archivo `.class` para que el bytecode sea interpretado al código de la máquina.

<br/>
![proceso de JDK](/img/wiki/JDKvsJRE.png)
<br/>

Tu IDE integra todas estas herramientas de manera que cada vez que grabás un archivo Java realiza la compilación para generar el `.class` y ejecutarlo desde el mismo entorno. Por eso recordemos que un IDE es un _Integrated Development Environment_.

## Desarrollo en Kotlin/JVM

Haciendo la aclaración de que hay variantes de Kotlin que no necesitan la JDK (Kotlin Native o bien Kotlin/JS), en las materias Algoritmos 2, Algoritmos 3 y Programación con Herramientas Modernas trabajamos con Kotlin/JVM que precisan instalarse la JDK.

De esa manera cuando generamos nuestro archivo `.kt` con el código fuente, el compilador automáticamente genera el bytecode asociado (el `.class`, que está en la carpeta `build/classes/kotlin`) utilizando tanto el compilador de Kotlin como las herramientas que trae la JDK.

<br/>
![proceso de desarrollo en Kotlin](/img/wiki/Kotlin_JDK_JRE.png)
<br/>

Como estamos trabajando dentro de un IDE, este paso adicional es transparente para nosotros. Solo debemos tener en cuenta que además del plugin de Kotlin, necesitaremos tener instalada una JDK para que el proceso de fondo convierta los archivos `.kt` a `.class`.

Como resultado, nuestros programas pueden ejecutarse utilizando la máquina virtual de Java a partir de cualquier JRE.

## Links relacionados

- [Volver al menú principal del entorno Kotlin](kotlin-principal.html)
- [Página principal de Algoritmos 2](algo2-temario.html)
