---
layout: article
title: Preparacion de un entorno de desarrollo java
categories: [java, enviroment]
featured: true
---

<img src="/img/languages/Java_logo.png" width="30%" height="30%">

# Introducción

Para realizar aplicaciones de complejidad mediana-grande en Java, es recomendable contar con un entorno de trabajo que contemple al menos:

-   Una herramienta de versionado de fuentes
-   Una herramienta de manejo de dependencias
-   Un mecanismo para automatizar los procesos administrativos del desarrollo (test, release, deploy, etc)
-   Un entorno de programación que permita:
    -   Ayudas a la detección temprana de errores, autocompleción, herramientas para navegar y buscar ágilmente dentro del código, etc.
    -   Soporte para la realización de refactors automatizados
    -   Integración con la mayor cantidad posible de las demás herramientas que utilizamos.

En este artículo se propone una configuración de entorno de trabajo que intenta cumplir con los anteriores objetivos. Las herramientas seleccionadas para eso son:

-   **Java Development Kit**
-   **Eclipse** como entorno integrado de desarrollo
-   **Git** como repositorio de fuentes y herramienta de versionado
-   **Maven** como herramienta para manejar dependencias y automatizar diversos procesos administrativos.

Adicionalmente se instalarán extensiones al entorno de desarrollo eclipse para integrarlo con git y maven.

# JDK (Java Development Kit)

Contiene un compilador y una máquina virtual (el runtime) que traduce a código de máquina el código intermedio que genera el compilador (.java → COMPILADOR (javac.exe) → .class → VM (java.exe) → ejecutable final).

Al tiempo de escribir este artículo la última versión estable es 1.8. Para el propósito aquí descripto es recomendable instalar la *Standard Edition*.

# Download e instalación base

A continuación se detallan los pasos básicos de instalación según el sistema operativo que se esté utilizando. Luego de realizar este paso inicial se deberá pasar a la configuración del entorno.

Este es el link de [downloads](http://www.oracle.com/technetwork/java/javase/downloads/index.html).

Desde ahí buscan el Latest Release y se descargan el JDK del sistema operativo que esté instalado en sus máquinas.

Para más detalles adicionales a los que se encuentran en esta página, se puede consultar el [manual de instalación de sun](http://java.sun.com/javase/6/webnotes/install/index.html).

## Ubuntu

Para instalarlo en Ubuntu se puede hacer:
``` bash
$ sudo apt-get install openjdk-8-jdk
```

Para definir la versión por defecto, podés correr 
``` bash
$ sudo update-alternatives --config java
```

y definir la versión con la que vas a usar.

## Otros sistemas operativos

Para otros sistemas operativos desde [este link](http://www.oracle.com/technetwork/java/javase/downloads/index.html) seguís [la explicación paso por paso que se encuentra aquí](http://docs.oracle.com/javase/8/docs/technotes/guides/install/install_overview.html).

# Documentación

-   [Java Tutorials](http://java.sun.com/docs/books/tutorial/)
-   [Javadoc reference guide](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/javadoc.html)
-   [Writing comment tips](http://java.sun.com/j2se/javadoc/writingdoccomments/index.html)


# Eclipse

La instalación del eclipse es muy sencilla: hay que bajar el que corresponda a su sistema operativo desde <http://www.eclipse.org/downloads/> y descomprimirlo en su disco rígido. Posiblemente deseen crear un acceso directo para apuntar al ejecutable. 

A los efectos de los objetivos planteados en este artículo, se recomienda elegir la versión denominada "Eclipse IDE for Java EE Developers".

Esa versión pesa bastante. Si no van a utilizar las herramientas de programación web es posible utilizar la versión más liviana "Eclipse IDE for Java Developers".


## Configuraciones adicionales

-    Te recomendamos chequear [estas configuraciones de Eclipse](/wiki/articles/configuraciones-generales-para-cualquier-eclipse.html)


## Documentación

-   [Página principal de Eclipse](http://www.eclipse.org/)


# Maven

Para instalar Maven te recomendamos [seguir las instrucciones de esta página](/wiki/articles/guia-de-instalacion-de-maven.html)


## Creación de un proyecto básico

Una vez instaladas todas las herramientas, se puede crear un proyecto en esta plataforma siguiendo [este tutorial](creacion-de-un-proyecto-maven-basico.html) (ojo, este es un tutorial básico, si necesitan usar otras tecnologías de presentación busquen los tutoriales en las páginas de las tecnologías correspondientes).

## Mavenizar un proyecto existente

Si ya arrancaste tu proyecto y decidiste más tarde que necesitabas usar Maven, podés *mavenizarlo* de alguna de las siguientes maneras.

### Usando M2Eclipse

-   Click derecho en el Proyecto

<!-- -->

-   *Configure* -&gt; *Convert to Maven Project*

<!-- -->

-   Ingresar el **groupId deseado**. Preferentemente que sea el package name que se hayan definido antes, ej: *org.uqbar.arena.exampes*

<!-- -->

-   *Finish*

Independientemente de si su proyecto respeta la estructura estándar de un proyecto Maven, lo van a tener configurado y funcionando. De hecho, si crean los directorios y mueven el paquete a src/main/java, Maven automágicamente lo va a detectar.

``` bash
.
├── src
│   └── main
│       ├── java
│       │   └── org
│       │       └── uqbar
│       │           └── arena
│       │               └── examples
│       │                    └── Main.java
│       └── resources
├── pom.xml
```

<!-- -->

-   Probamos hacer una compilación e instalación local.


# Links útiles

-   [Amigandonos con el entorno de desarrollo](amigandonos-con-el-entorno-de-desarrollo.html)

