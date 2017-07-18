---
layout: article
title: Configuracion de arena
categories: [arena, entorno, instalacion]
featured: true
---

<img src="/img/languages/uqbar-arena-logo.png" height="30%" width="30%"/>

# Pre-requisitos

Asumimos que tenés instaladas las herramientas básicas según se explica en [este instructivo](preparacion-de-un-entorno-de-desarrollo-java.html), incluyendo Maven.

# Estructura de un proyecto Arena

En general una aplicación arena consta de dos componentes o partes:

-   el dominio
-   la vista

Si bien podemos tener todas las clases de ambas partes en un único proyecto, lo más prolijo es plasmar esa separación en dos proyectos (java/scala/loquesea). Porque por ejemplo, para el TP podrán reutilizar el dominio entre las diferentes tecnologías.

# Dependencias de los proyectos

Entonces en ese esquema si seguimos con la idea de que el dominio no debe estar acoplado a la tecnología de la vista directamente, nuestro proyecto "dominio" no dependerá del artefacto arena, pero sí de otro artefacto más abstracto que establece contratos de los objetos de negocio: uqbar-domain.

Por otro lado, nuestro proyecto que tendrá la vista en arena sí, lógicamente dependerá del artefacto "arena".

La siguiente figura resume esto:

{% link_image Arena-dependencias.fw.png %}

Nota: si hacemos un único proyecto con dominio + ui arena, podemos sólo depender de "arena" y transitivamente veríamos a "uqbar-domain".

# Lo que deben tener tus pom.xml

## Dependencias para proyectos de dominio

Si vas a definir tus objetos de dominio en un proyecto aparte (cosa que recomendamos) tenés que definir esta dependencia

```xml
<dependency>
  <groupId>org.uqbar-project</groupId>
  <artifactId>uqbar-domain</artifactId>
  <version>3.6.3</version>
</dependency>
```

## Dependencias para proyectos de UI

Agregar dos referencias en el pom

-   al framework Arena (consideramos Arena = Arena-JFace como predeterminado)
-   al proyecto de dominio (asumimos que vamos a tener dos proyectos separados, uno para el dominio y otro para la ui).

Por ejemplo, las dependencias en nuestro pom podrían quedar así:

```xml
<dependencies>
  <dependency>
    <groupId>org.uqbar-project</groupId>
    <artifactId>arena-jface</artifactId>
    <version>3.6.3</version>
  </dependency>
  <dependency>
    <groupId>uqbar-project.org</groupId>
    <artifactId>videoclub-domain</artifactId>
    <version>1.0.0-SNAPSHOT</version>
  </dependency>
</dependencies>
```

Las otras dependencias como JUnit se toman de la definición del parent project, en caso de ser necesario se debe agregar a mano:

```xml
<dependency>
  <groupId>junit</groupId>
  <artifactId>junit</artifactId>
  <version>4.12</version>
  <scope>test</scope>
</dependency>
```

Si no querés tocar el pom.xml a mano, podés agregarlo a través del plugin M2clipse: botón derecho sobre el proyecto, Maven &gt; Add Dependency &gt; buscás "arena" y tiene que aparecer "arena-jface", buscás la versión que querés (o si tenés dudas la última) y aceptás. Entonces el plugin va a descargarlo (si no lo tiene en tu repositorio local). Lo mismo con las demás dependencias que necesites.

# Arena y Xtend

Asumimos que además del entorno básico ya te instalaste Xtend según [este instructivo](preparacion-de-un-entorno-de-desarrollo-xtend.html).

## Crear un proyecto de Arena en Xtend

Si estás trabajando Arena-UI desde xtend, este parent contiene todas las dependencias que necesitás (JUnit, el compilador Xtend, Arena UI, Uqbar Domain, etc.):

```xml
<parent>
  <groupId>org.uqbar-project</groupId>
  <artifactId>arena-xtend-parent</artifactId>
  <version>3.6.3</version>
</parent>
```


# Integración con Scala

Asumimos que además del entorno básico ya te instalaste Scala según el instructivo para [Preparacion de un entorno de desarrollo Scala](preparacion-de-un-entorno-de-desarrollo-scala.html).


## Crear un proyecto de Arena en Scala

Las instrucciones para tener un proyecto Scala que utilice Arena son las mismas que para cualquier [Proyecto Maven con Scala](preparacion-de-un-entorno-de-desarrollo-scala-creacion-de-un-proyecto-maven-con-scala.html)

En caso de duda podés utilizar alguno de nuestros pom.xml como ejemplo.


# Integración con Groovy

Asumimos que además del entorno básico ya te instalaste Groovy según [este instructivo](preparacion-de-un-entorno-de-desarrollo-groovy.html).


## Crear un proyecto de Arena en Groovy

A las configuraciones generales del pom.xml (referenciar al parent-project y las dependencias de arena + junit), hay que incorporar:

-   Dependencias: el plugin para groovy

```xml
<dependencies>
  <dependency>
    <groupId>org.codehaus.mojo</groupId>
    <artifactId>build-helper-maven-plugin</artifactId>
    <version>1.8</version>
  </dependency>
  <dependency>
    <groupId>uqbar</groupId>
    <artifactId>arena-jface</artifactId>
    ...
</dependencies>
```

-   Y definir el directorio donde están los fuentes como src/main/groovy en lugar de src/main/java que es el default para Maven:

```xml
<build>
  <sourceDirectory>src/main/groovy</sourceDirectory>
  <testSourceDirectory>src/test/groovy</testSourceDirectory>
  ...
```

Podés utilizar alguno de nuestros pom.xml como ejemplo.

# Configurar launcher

En Run &gt; Run Configurations... &gt; Java Application &gt; New launch configuration (buscá el botón de la toolbar que está a la izquierda) y en la solapa Arguments, tenés que indicarle en VM Arguments que use el Launcher propio de Arena:

```bash
-Djava.system.class.loader=org.uqbar.apo.APOClassLoader
```

de lo contrario te va a aparecer un mensaje de error:

```bash
Exception in thread "main" java.lang.RuntimeException: Esta aplicación no está corriendo con el ClassLoader necesario. Corra  la aplicación con el siguiente parámetro para la VM: -Djava.system.class.loader=org.uqbar.apo.APOClassLoader. El ClassLoader actual es: sun.misc.Launcher$AppClassLoader@6fd3633c
   at org.uqbar.arena.Application.validateClassLoader(Application.java:32)
   at org.uqbar.arena.Application.`<init>`(Application.java:24)
```

En muchos ejemplos tenemos un archivo .launch que tiene esta configuración ya cargada.

# Troubleshooting

¿Qué hacer cuando nos bajamos ejemplos (o desarrollamos uno nuevo) y no nos andan? Chequear esta lista...

## Maven

Revisá que Maven esté correctamente instalado en tu máquina y que tenés el settings.xml correctamente configurado. Cualquier duda fijate en [el tutorial de instalación de Maven](guia-de-instalacion-de-maven.html).

También asegurate que la versión de Maven sea 3.0.x o posterior, o vas a tener un mensaje de error similar a éste:

```bash
[INFO] Unable to initialise extensions
Component descriptor role: 'com.jcraft.jsch.UIKeyboardInteractive', implementation: 'org.apache.maven.wagon.providers.ssh.jsch.interactive.PrompterUIKeyboardInteractive', role hint: 'default' has a hint, but there are other implementations that don't
```

## Source folders del proyecto

Los source folders de los proyectos (que tienen maven como estructura central del proyecto) deben ser

-   para proyectos Java o Xtend: src/main/java y src/test/java
-   para proyectos Groovy: src/main/groovy y src/test/groovy
-   y para proyectos Scala es src/main/scala y src/test/scala.

Si te aparece como source folder sólo el src, o bien si no tenés source folders: 

1. corré el plugin de maven: botón derecho sobre el proyecto &gt; Configure &gt; Maven project (o mvn compile) 

2. si el proyecto tiene la naturaleza Maven (aparece una M sobre el nodo del proyecto en el Project Explorer), probá hacer un Maven &gt; Update project

3. revisá si efectivamente tenés una estructura de directorio src/main/**lenguaje**, en base al lenguaje que estás utilizando. Esa estructura debe respetarse.


## JDK

Revisá que tengas instalada una JDK (no JRE, tiene que ser JDK con las herramientas para desarrollar en Java como el compilador, debug, etc.) y que la versión de ese JDK sea 1.8 ó superior. Si querés usar una JDK 1.7 ó inferior te va a aparecer el siguiente mensaje de error

```bash
java.lang.UnsupportedClassVersionError: ---aplicación de Arena--- : Unsupported major.minor version 51.0
```

porque Arena está compilado con una JDK 1.8

## Problemas para encontrar la ventana ejecutable

Si te aparece un error similar a éste al correr un "launcher":

```bash
Error: no se ha encontrado o cargado la clase principal org.uqbar.arena.examples.conversor.xtend.ConversorWindow
```

Revisá

-   si tenés correctamente definidos los source folders (punto anterior)
-   si tenés las librerías de Xtend/Groovy/Scala importadas en tu proyecto (sólo la del lenguaje en el que te lo bajaste, claro está). En caso de que no sea así, se pueden agregar manualmente, aunque lo mejor es investigar por qué el pom no generó correctamente la asociación de la librería del lenguaje en tu proyecto Eclipse
-   por último, si tenés apuntado en tu proyecto un JDK válido (que apunte a un directorio que exista en tu PC, revisá Window &gt; Preferences &gt; Installed JREs)


## Problemas al correr el ejecutable

Si te aparece un error como éste al correr un launcher:

```bash
Error occurred during initialization of VM
java.lang.Error: java.lang.ClassNotFoundException: org.uqbar.arena.aop.ArenaClassLoader
   at java.lang.ClassLoader.initSystemClassLoader(Unknown Source)
   at java.lang.ClassLoader.getSystemClassLoader(Unknown Source)
Caused by: java.lang.ClassNotFoundException: org.uqbar.arena.aop.ArenaClassLoader
   at java.net.URLClassLoader$1.run(Unknown Source)
```

El problema está en el launcher, el ArenaClassLoader está deprecado, se reemplazó por el APOClassLoader (más arriba te dice cómo configurarlo).

## APOClassLoader not found

Otro problema que puede aparecer es:

```bash
Error occurred during initialization of VM
java.lang.Error: java.lang.ClassNotFoundException: ....APOClassLoader
```

entonces el problema puede darse porque no te descargó las dependencias de Maven correctamente. Revisá los directorios de tu usuario/.m2/repository porque seguramente te falte bajar dependencias, podés probar haciendo Maven &gt; Update project (forzando el check Update snapshots), es poco probable que eso lo solucione pero al menos te puede ayudar a encontrar el origen de la dependencia errónea.


## Errores de launchers

Otro problema que te puede ocurrir cuando corras un launcher que te descargaste de nuestros ejemplos es que te aparezca un error de este tipo:

```bash
Launch configuration references non-existing project celulares-ui-arena-scala
```

En este caso el problema es que te descargaste el proyecto del repositorio utilizando otro nombre que el que originalmente definimos. Entonces fijate cuál es el nombre del proyecto que está esperando y renombralo a ese, o bien entrá por el menú Run Configuration y apuntá el launcher al proyecto que vos definiste. Otra opción puede ser que no hayas ejecutado el comando mvn compile (Run As &gt; Maven build... Goal compile)


## Problemas específicos con Groovy

Si te aparece un error como el siguiente:

```bash
groovy-all is loaded in version 2.0.6 and you are trying to load version 2.1.6
```

Editá el pom.xml y cambiale la versión de groovy-all a 2.0.6 (o la versión en la que esté cargada)


# Links relacionados

-  [Temario Algoritmos III](algo3-temario.html)
