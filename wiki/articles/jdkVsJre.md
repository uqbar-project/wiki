---
layout: article
title: JDK vs. JRE
---

![jdk set of tools](/img/wiki/JDKandJREandJVM.png)

Cuando nos piden "instalar Java" (a secas) en nuestra máquina, es importante tener en claro la diferencia entre:

- **JRE** o _Java Runtime Environment_: un conjunto de herramientas que permite ejecutar código compilado en Java. La noción de _environment_ o ambiente donde viven los objetos está dado por la JVM (_Java Virtual Machine_).
- **JDK** o _Java Development Kit_: incluye al JRE y además le agrega herramientas propias para desarrollar en Java: el compilador (`javac`), el generador de documentación html para las clases (`javadoc`), el debugger (`jdb`), entre otros.

Para tener el listado completo de las herramientas que trae OpenJDK, recomendamos ingresar a [esta página](https://openjdk.java.net/tools/index.html).

## Arquitectura general del entorno Java

En este diagrama vemos el proceso de desarrollo de un programa Java desde que lo escribimos hasta que se ejecuta en una máquina (nuestra o de un usuario). Para compilar el programa Java a un código intermedio o _bytecode_ necesitamos el ejecutable `javac`, que viene con el JDK. El archivo `.class` generado puede ser interpretado en cualquier otro sistema operativo, solo necesitamos tener el JRE adecuado. Ejecutamos entonces el programa `java` (o `javaw` en Windows) pasando como argumento nuestro archivo `.class` para que el bytecode sea interpretado al código de la máquina.

<br/>
![proceso de JDK](/img/wiki/JDKvsJRE.png)
<br/>

Eclipse integra todas estas herramientas de manera que al grabar realiza la compilación para generar el `.class` y poder ejecutarlo desde el mismo entorno (de ahí su nombre IDE, o _Integrated Development Environment_).

## Desarrollo en Xtend

Xtend agrega un paso previo: nosotros escribimos en Xtend, al grabar el archivo se genera el `.java` y a su vez el `.class`.

<br/>
![proceso de desarrollo en Xtend](/img/wiki/xtendJDKvsJRE.png)
<br/>

Como estamos trabajando dentro de Eclipse, este paso adicional es transparente para nosotros. Solo debemos tener en cuenta que necesitaremos tener una JDK para que el proceso de fondo convierta los archivos `.xtend` a `.java` y luego a `.class`.

Como resultado, nuestros programas pueden ejecutarse utilizando la máquina virtual de Java a partir de cualquier JRE.

## Links relacionados

- [Volver al menú principal del entorno Xtend](xtend-principal.html)
- [Página principal de Algoritmos 2](algo2-temario.html)
