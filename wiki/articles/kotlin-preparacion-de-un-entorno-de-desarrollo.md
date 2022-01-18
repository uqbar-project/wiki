---
layout: article
title: Kotlin - Preparacion del entorno de desarrollo
categories: [kotlin, entorno, instalacion]
featured: true
---

<img src="/img/wiki/Kotlin_logo.png" height="30%" width="30%">

# Instalación base

## Git Bash (sólo para Windows)

Para simplificar el uso de Git en entornos Windows, existe la herramienta **Git Bash** que podés descargar a partir de [esta página](https://gitforwindows.org/), haciendo click en el link "Download".

Si estás en Mac o Linux, podés saltear este paso.

## JDK: Java Development Kit

Primero instalaremos el compilador de Java.

Ingresamos a [esta dirección](https://jdk.java.net/java-se-ri/14), y descargamos la **Open JDK 14**, que es la versión oficial que manejamos desde 2022 y cuya licencia es [GPL](https://es.wikipedia.org/wiki/GNU_General_Public_License). 


### Pasos de instalación

Una vez descargado el binario en una carpeta (supongamos que es `C:\jdk14`), hay que configurar dos variables de entorno de tu sistema operativo:

- JAVA_HOME: tiene que apuntar a `C:\jdk14`)
- PATH: hay que incorporarle `C:\jdk14\bin` (cuidando de no borrar lo que ya está)

Te dejamos [un video que explica cómo hacerlo para Windows](https://www.youtube.com/watch?v=1guhEwxMAz8&ab_channel=Zurck%27zZ%C3%B6%C3%AB) (el procedimiento es similar para MacOS / Linux)


### Chequeos posteriores a la instalación

- Dentro de las variables de entorno de tu sistema operativo debe estar JAVA_HOME asignada. En Linux / Mac esto es `env | grep JAVA_HOME`, y en Windows `SET JAVA_HOME`. **Si la variable no está seteada, eso significa que te salteaste un paso, lo mismo si la carpeta que muestra JAVA_HOME no es la que contiene la versión que vos descargaste**. En ese caso volvé al punto anterior y seguí nuevamente las instrucciones para encontrar lo que está faltando. 
- En una ventana de línea de comandos, verificar la versión de java instalada con `java -version`, y el compilador mediante `javac -version`. En ambos casos mostrará la versión por defecto para tu máquina. **Si no aparece la versión que descargaste, el sistema operativo asume por defecto otra instalación, que podría ser incluso de una JRE (ver más abajo)**. En ese caso, revisá el link del punto anterior para ver qué puede estar faltando y repetí los pasos.

### JDK sí, JRE no

> **IMPORTANTE:** tenés que instalar una JDK, no una JRE (Java Runtime Environment) que solo te permite ejecutar programas Java ya compilados. Para saber si tenés una JDK, deberías ir al directorio de instalación y en la carpeta `bin` debe estar un programa llamado `javac`, que es el compilador de Java.

![image](/img/languages/jdkVsJre.png)

Si no tenés ese programa, no vas a poder pasar tus objetos a código ejecutable en el entorno Kotlin: la solución es muy simple, descargá e instalá una JDK. Para más información te recomendamos [esta página](jdkVsJre.html)

## IntelliJ IDEA

Nuestro entorno integrado de desarrollo (IDE) permite que en una misma herramienta editemos nuestro código fuente, compilemos, hagamos pruebas, y muchas cosas más. En Algoritmos 1 ya conociste Eclipse, con un entorno modificado especialmente para soportar el lenguaje Wollok. Aquí utilizaremos IntelliJ IDEA que es un entorno bastante similar.

### Pasos de instalación

Tenés que descargarlo desde [esta página](https://www.jetbrains.com/es-es/idea/download/) que te va a ofrecer dos opciones:

- **Ultimate**: es la versión que recomendamos, para obtener una licencia podés enviar una solicitud con tu cuenta de UNSAM (debe terminar en `@unsam.edu.ar` ya que de esa manera se comprueba el origen educativo de la cuenta) [**siguiendo los pasos que cuenta esta página**](https://www.jetbrains.com/es-es/community/education/#students).
- **Community**: es la versión open-source que no tiene disponibles los plugins para Spring, herramienta que vemos en Algoritmos 3 y Programación con Herramientas Modernas.


### Chequeos de instalación

Una vez que te descargaste el instalable, solo tenés que

- elegir la ruta donde va a quedar el ejecutable (podés dejar la que te ofrece el instalador)
- chequear la opción para que te aparezca el link al ejecutable IntelliJ (_64-bit launcher_)
- las otras opciones no son necesarias activarlas

y finalmente presionar `Next` hasta terminar el asistente.

Necesitarás definir un espacio de trabajo o _workspace_, que es la carpeta donde vas a ubicar todos tus proyectos. Por defecto ese directorio es `~/IdeaProjects` donde `~` es tu carpeta personal (como `C:\Users\fernando` o `/home/fernando`).

### Configuraciones adicionales

Por lo general no es necesario hacer nada más, solo en algunos casos en los que notes que funciona lento, podés configurar el tamaño de memoria de la Virtual Machine de Java mediante el menú `Edit > Custom VM Options`. Esto abre un archivo de texto similar al siguiente

```ini
-Xms128m
-Xmx2048m
-XX:ReservedCodeCacheSize=512m
-XX:+UseConcMarkSweepGC
-XX:SoftRefLRUPolicyMSPerMB=50
-XX:CICompilerCount=2
-XX:+HeapDumpOnOutOfMemoryError
-XX:-OmitStackTraceInFastThrow
-ea
-Dsun.io.useCanonCaches=false
-Djdk.http.auth.tunneling.disabledSchemes=""
-Djdk.attach.allowAttachSelf=true
-Djdk.module.illegalAccess.silent=true
-Dkotlinx.coroutines.debug=off
-Dsun.tools.attach.tmp.only=true
```

Los parámetros que tenés que revisar son:

- la memoria inicial con la que levanta IntelliJ: `Xms`
- la memoria máxima que puede ser utilizada para IntelliJ, que corre en una Virtual Machine de Java propia: `Xmx`. Por defecto viene con 2GB y para las necesidades de la materia no deberías tener que subirlo, pero en todo caso charlalo con tu docente favorito.

Para más información podés chequear [esta página](https://intellij-support.jetbrains.com/hc/en-us/articles/206544869-Configuring-JVM-options-and-platform-properties).


## Plugin Kotest

Solo necesitamos agregar un plugin para ejecutar los tests unitarios: **Kotest**. Para instalarlo podés ir a [esta página](https://plugins.jetbrains.com/plugin/14080-kotest) y clickear el botón `Install to IntelliJ xxx` donde xxx es la versión más reciente que hayas instalado.

## Actualizaciones automáticas

Una vez instalado IntelliJ, las actualizaciones se disparan automáticamente. Por lo general es conveniente esperar un tiempo prudencial, sobre todo si tenés fechas de entrega de trabajos prácticos o un parcial en esos días, ya que ha habido algunos inconvenientes de uso que se suelen solucionar rápido pero no hace falta agregar más adrenalina a la cursada. La estrategia conservadora es la que te invitamos a seguir: esperar dos o tres semanas hasta que tu docente confirme que no hay problemas en instalar el _update_ y estar con buena señal de Internet para iniciar ese proceso.

<!-- -->

# Links útiles

- [Siguiente paso: Creación de un proyecto en Kotlin](kotlin-creacion-proyecto.html)
- [Volver al menú principal del entorno Kotlin](kotlin-principal.html)
