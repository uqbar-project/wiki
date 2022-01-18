---
layout: article
title: Preparacion de un entorno de desarrollo xtend
categories: [xtend, entorno, instalacion]
featured: true
---

<img src="/img/wiki/Xtend-logo.png" height="30%" width="30%">

# Download e instalación base

## Git Bash (sólo para Windows)

Para simplificar el uso de Git en entornos Windows, existe la herramienta **Git Bash** que podés descargar a partir de [esta página](https://gitforwindows.org/), haciendo click en el link "Download".

Si estás en Mac o Linux, podés saltear este paso.

## JDK: Java Development Kit

Primero instalaremos el compilador de Java.

Ingresamos a [esta dirección](https://jdk.java.net/java-se-ri/11), y descargamos la **Open JDK 11**, que es la versión oficial que manejamos desde 2020 y cuya licencia es [GPL](https://es.wikipedia.org/wiki/GNU_General_Public_License). 


### Pasos de instalación

Una vez descargado el binario en una carpeta (supongamos que es `C:\jdk11`), hay que configurar dos variables de entorno de tu sistema operativo:

- JAVA_HOME: tiene que apuntar a `C:\jdk11`)
- PATH: hay que incorporarle `C:\jdk11\bin` (cuidando de no borrar lo que ya está)

Te dejamos [un video que explica cómo hacerlo para Windows](https://www.youtube.com/watch?v=Cr_mwn67kFs) (el procedimiento es similar para MacOS / Linux)


### Chequeos posteriores a la instalación

- Dentro de las variables de entorno de tu sistema operativo debe estar JAVA_HOME asignada. En Linux / Mac esto es `env | grep JAVA_HOME`, y en Windows `SET JAVA_HOME`. **Si la variable no está seteada, eso significa que te salteaste un paso, lo mismo si la carpeta que muestra JAVA_HOME no es la que contiene la versión que vos descargaste**. En ese caso volvé al punto anterior y seguí nuevamente las instrucciones para encontrar lo que está faltando. 
- En una ventana de línea de comandos, verificar la versión de java instalada con `java -version`, y el compilador mediante `javac -version`. En ambos casos mostrará la versión por defecto para tu máquina. **Si no aparece la versión que descargaste, el sistema operativo asume por defecto otra instalación, que podría ser incluso de una JRE (ver más abajo)**. En ese caso, revisá el link del punto anterior para ver qué puede estar faltando y repetí los pasos.

### JDK sí, JRE no

> **IMPORTANTE:** tenés que instalar una JDK, no una JRE (Java Runtime Environment) que solo te permite ejecutar programas Java ya compilados. Para saber si tenés una JDK, deberías ir al directorio de instalación y en la carpeta `bin` debe estar un programa llamado `javac`, que es el compilador de Java.

![image](/img/languages/jdkVsJre.png)

Si no tenés ese programa, no vas a poder pasar tus objetos a código ejecutable en el entorno Xtend: la solución es muy simple, descargá e instalá una JDK. Para más información te recomendamos [esta página](jdkVsJre.html)

## Eclipse

Nuestro entorno integrado de desarrollo (IDE) permite que en una misma herramienta editemos nuestro código fuente, compilemos, hagamos pruebas, y muchas cosas más. En Algoritmos 1 ya conociste Eclipse, con un entorno modificado especialmente para soportar el lenguaje Wollok. Aquí lo utilizaremos con diferentes plugins, pero seguramente te resultará familiar la forma de trabajar.

### Pasos de instalación

Tenés que descargarlo desde [esta página](https://www.eclipse.org/downloads/) utilizando el link más reciente de Eclipse (al 01/03 es **Get Eclipse IDE 2020‑12**)

> **NOTA:** podés visitar [la página histórica de descarga de Eclipses anteriores](https://wiki.eclipse.org/Older_Versions_Of_Eclipse) por si hay versiones más nuevas y te interesa usar la versión 2020-12 que es con la que desarrollamos los ejemplos. Normalmente las actualizaciones de Eclipse siguen funcionando sin mayores problemas con el plugin de Xtend, Java y Maven.

Eso te descarga un eclipse-installer, que es el primer paso. Lo abrís con un doble click, y luego seleccionás

- si estás cursando Programación con Herramientas Modernas: "Eclipse IDE for Enterprise Java Developers"
- cualquier otra cursada: "Eclipse for Java Developers", seleccionando la carpeta de destino.

### Chequeos de instalación

Una vez que lo hayas descomprimido en una carpeta, podés hacer un acceso directo al `eclipse` o `eclipse.exe` y ejecutarlo con doble click. Necesitarás definir un espacio de trabajo o _workspace_, que es la carpeta donde vas a ubicar todos tus proyectos.

### Configuraciones adicionales

Por lo general no es necesario hacer nada más, pero en caso de necesitarlo, en la carpeta raíz donde descargaste el Eclipse vas a encontrar un archivo `eclipse.ini` que permite configurar

f- cuál es la versión de Java requerida (por defecto es 1.11 y no debería ser necesario modificarla)
- cuál es la ubicación donde está el ejecutable de Java: es importante que apunte a una JDK y no a una JRE, como hemos comentado en la instalación de la JDK. Si por defecto instalaste una JRE, tu Eclipse no será capaz de compilar, recomendamos volver a la página JDK y reinstalar Java. De la misma manera, la JDK a la que apunte Eclipse debería ser la misma que vos instalaste: asegurate de que estén sincronizadas.

A continuación te dejamos un archivo `.ini` de ejemplo, ignorando las primeras líneas:

```ini
...
--launcher.appendVmargs
-vm
/usr/lib/jvm/java-11-openjdk-amd64/bin
-vmargs
-Dosgi.instance.area.default=@user.home/eclipse-workspace
-XX:+UseG1GC
-XX:+UseStringDeduplication
--add-modules=ALL-SYSTEM
-Dosgi.requiredJavaVersion=1.11
-Dosgi.dataAreaRequiresExplicitInit=true
-Xms512m
-Xmx1768m
--add-modules=ALL-SYSTEM
-Declipse.p2.max.threads=10
-Doomph.update.url=http://download.eclipse.org/oomph/updates/milestone/latest
-Doomph.redirection.index.redirection=index:/->http://git.eclipse.org/c/oomph/org.eclipse.oomph.git/plain/setups/
```

En el ejemplo estamos configurando una memoria inicial de 512 MB y una memoria máxima de 1768 MB, una JDK 1.11 requerida. El resto son valores por defecto que te va a crear el instalador de Eclipse.

## Maven

Seguí los pasos de instalación de [esta página](guia-de-instalacion-de-maven.html)

## Plugin Xtend

Instalá el plugin de Xtend desde el Update Site, siguiendo estos pasos:

- En el menú de Eclipse, Help > Install New Software ... botón Add
- En la ventana de diálogo Add Repository, en el nombre escribir algo como "Xtend Plugin" y en Location copiar esta URL: [http://download.eclipse.org/modeling/tmf/xtext/updates/milestones/](http://download.eclipse.org/modeling/tmf/xtext/updates/milestones/)

- A partir del 2021 se estará usando la versión **2.25**, en caso de que vayan saliendo nuevas versiones, se puede elegir qué versión instalar destildando la opción "Show only the latest versions of available software" (más abajo está resaltado en la imagen). En caso de duda sobre qué versión instalar, aconsejamos consultar por la lista de difusión al docente responsable.
- Seleccionar el check Xtext, y luego Xtend IDE, hacer click en Next y luego en Finish

![image](/img/wiki/Xtend-installation-2020-2.png)

- Reiniciar el Eclipse

<!-- -->

# Configuraciones default del eclipse

Antes que nada chequeá las [Configuraciones generales para cualquier Eclipse](configuraciones-generales-para-cualquier-eclipse.html)

# ¿Cómo empezar?

- Crear un proyecto Maven (si no instalaste Maven hacelo como se sugiere [aquí](guia-de-instalacion-de-maven.html)
  - en la primera ventana, clickear en la opción "Create a simple project (Skip archetype selection)", luego Next...
  - definir un groupId, que puede ser el *materia* . Ej: algo2, phm, etc.
  - definir un artifactId, que se asocia al nombre de tu proyecto

- Para definir el archivo `pom.xml` de Maven con el que vas a configurar tu proyecto, podés basarte en el modelo de ejemplo de [esta página](xtend-creacion-proyecto.html) 

<!-- -->

# Tips

- Para que cuando hagas New > File te aparezcan las clases y las interfaces Xtend, Window > Customize Perspective... > solapa Menu Visibility > expandís File > New > y seleccionás las de Xtend (Xtend class, inteface, annotation y enum).

<!-- -->

# Documentación

- [Documentación oficial](http://www.eclipse.org/xtend/documentation/)

<!-- -->

# Links útiles

- Si venís del mundo Java chequeá [este link](http://jnario.org/org/jnario/jnario/documentation/20FactsAboutXtendSpec.html)
- [Siguiente paso: Creación de un proyecto en Xtend](xtend-creacion-proyecto.html)
- [Volver al menú principal del entorno Xtend](xtend-principal.html)
