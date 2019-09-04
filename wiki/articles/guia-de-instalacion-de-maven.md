---
layout: article
title: Guia de instalacion de maven
---

![image](../../img/languages/mavenLogo.png)

# Introducción

Maven es una herramienta que permite automatizar tareas de los desarrolladores, y facilitar el manejo de dependencias, la configuración de entornos de trabajos locales, entre muchas otras cosas. Por defecto, al instalar Eclipse también viene el plugin para utilizar Maven, pero a veces es necesario ejecutar instrucciones adicionales por consola y para eso son estas instrucciones.

## Instalación en Windows

Recomendamos seguir [este tutorial](https://www.mkyong.com/maven/how-to-install-maven-in-windows/), ignorando la versión de JDK que es indistinta.

## Instalación en SO Unix-based (Linux, Solaris y Mac OS X)

## Mediante apt-get

```bash
$ sudo apt-get install maven
$ sudo ln -s /usr/share/maven3/bin/mvn /usr/bin/mvn
```

Vale la pena aclarar que si bien este método es ms simple, generalmente las versiones de repositorio de Ubuntu suelen estar  atrás de las vigentes. Igualmente no les va a traer problemas.

## Manualmente

Descargar Apache Maven 3 desde [este link](http://apache.dattatec.com/maven/maven-3/3.6.0/binaries/apache-maven-3.6.0-bin.tar.gz).

Descomprimir el tarball y mover el directorio a donde usualmente se guardan los programas. Ejemplo: */home/john/programs/*.

```bash
$ tar -xzvf apache-maven-3.6.0-bin.tar.gz
$ mv apache-maven-3.6.0 /home/john/programs/
```

Agregar la siguiente línea al archivo **.bashrc**. Este archivo oculto (su nombre empieza con '.') contiene comandos que se ejecutan cuando se abre una terminal (consola). Se puede abrir con cualquier editor de textos (gedit, vim, emacs, notepad++, etc) y se encuentra en el directorio **home** del usuario.

```bash
export PATH=$PATH:$HOME/programs/apache-maven-3.5.0/bin
```

Una forma sencilla de hacer ésto (sin tener que abrir un editor) es usando el programa **echo** y *agregando al final* del archivo el valor ingresado. **Prestar atención al hecho de que se usan dos signos mayor**:

```bash
$ echo 'export PATH=$PATH:$HOME/programs/apache-maven-3.5.0/bin' >> .bashrc
```

## Chequeos posteriores a la instalación

Corroboramos que podemos usar Maven. El output sería algo parecido a éste:

```bash
john@notebook:~$ mvn -v
Apache Maven 3.5.0 (r...)
Maven home: /home/john/programs/apache-maven-3.6.0
Java version: 1.8...
Java home: /usr/lib/jvm/...
Default locale: en_US, platform encoding: UTF-8
OS name: "linux", version: "3.0.0-19-generic", arch: "i386", family: "unix"
```

## Configuración de Maven

Por defecto no necesitás hacer nada, la configuración por defecto está bien para comenzar a trabajar. Pero en caso de ser necesario algunos ajustes, tenés que mirar el archivo `settings.xml`, que por defecto se ubica en

- `home/usuario/.m2` para sistemas operativos Unix-based
- `C:\Users\Usuario\.m2` para Windows, donde C: es el drive donde se instaló Maven 
-  si quieren modificar el directorio por defecto, [podés chequear esta pregunta en stack overflow](https://stackoverflow.com/questions/16649420/how-to-specify-an-alternate-location-for-the-m2-folder-or-settings-xml-permanen).
-  si el archivo no existe, tenés que crearlo. Para más información te dejamos [la documentación oficial de la configuración de Maven](https://maven.apache.org/settings.html)

## Propiedades de Maven en Eclipse

Window &gt; Preferences te permite configurar algunas propiedades para Maven. Te recomendamos

- tener chequeado "Do not automatically update dependencies from remote repositories" para que no intente bajarte permanentemente nuevas versiones de los componentes que utilices. Esto requiere que lo hagas en forma manual, algo que quizás sea más recomendable.
- tener chequeado "Download artifact sources" te permite ver el código fuente de los .jars que te bajes, esta opción hace que las descargas inicialmente tarden un poco más de tiempo pero es bueno cuando tenés algún error y necesitás entender cómo funciona alguna parte de un componente.
- también es bueno chequear "Download artifact javadocs" para obtener documentación de los componentes que utilizamos
- Y por último tener deschequeada la opción "Update Maven projects on startup" permite que manualmente vos actualices los proyectos solamente ante un cambio y no cuando levantes el Eclipse.

Una configuración más que puede ser útil para encontrar versiones nuevas de artefactos en los repositorios es dejar chequeada:

- La opción "Download repository index on startup" (opción por defecto chequeada): para más información pueden leer <http://stackoverflow.com/questions/8647769/what-is-eclipse-doing-when-it-says-that-its-updating-indexes>.

# Material

- [Guía de referencia rápida](https://maven.apache.org/guides/MavenQuickReferenceCard.pdf)
