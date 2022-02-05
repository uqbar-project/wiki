---
layout: article
title: Introducción a Gradle
categories: [gradle, build, integración, scm]
featured: true
---

![Gradle logo](/img/wiki/gradle_logo.png)

Gradle es una herramienta que ayuda a desarrollar un proyecto basado en el entorno de una JDK (Java, Kotlin, Xtend, Scala, Groovy, etc.)

## Funcionalidades

Gradle cumple con las siguientes funciones principales que vamos a explicar en las siguientes secciones:

- Reificación del Proyecto
- Manejo de Dependencias
- Tareas

## Reificación de Proyecto

Java no trabaja la idea de proyecto, no lo representa como concepto, y esto se traslada a todos los lenguajes basados en la JDK. Entonces, cada uno de los IDEs pensados para Java agregan su propia forma de definirlo: 

- en Eclipse tenemos los archivos `.classpath` y `.project`
- en IntelliJ tenemos el directorio oculto `.idea`
- en Visual Studio Code tenemos el directorio `.vscode`

Gradle permite trabajar en cualquiera de estos IDEs con su propio modelo de proyecto, que se guarda en el archivo `build.gradle.kts` en el caso de trabajar utilizando el lenguaje Kotlin o `build.gradle` a secas, cuando la configuración de Kotlin se hace utilizando el lenguaje Groovy. A partir de ahora vamos a continuar la explicación asumiendo que estás usando Gradle con Kotlin.

> **Aclaración:** A partir de ahora estaremos utilizando los términos `componente` y `artefacto` como sinónimos de nuestro proyecto.

## Identificación de un proyecto Gradle

Todo proyecto en Gradle tiene tres elementos que lo ayudan a ser identificado de forma única entre todos los proyectos existentes a nivel global:

- **group**: representa la organización autora/dueña del artefacto. Por ejemplo, los proyectos de Algoritmos 2 suelen usar el groupId `ar.edu.unsam.algo2`.
- **artifact**: este campo define el nombre por el que se conoce al proyecto en sí mismo. Algunos ejemplos: `commons-collections`, `eg-seguros-xtend`, `tp-futbol5-grupo01`, etc.
- **version**: es el último componente del rompecabezas, dado que groupId:artifactId denota un único proyecto pero no alcanza para definir en qué versión del proyecto nos estamos parando. Se agrega entonces un número de versión para completar la información que Gradle necesita para generar una identificación unívoca. Conviene seguir las reglas de [**versionado semántico**](https://semver.org/), para liberar versiones productivas. A veces se suele acompañar de un sufijo `RELEASE` (para versiones estables) o `SNAPSHOT` (para versiones intermedias que pueden estar sujetas a cambios).

Gradle tomó esta misma forma de identificar un proyecto que Maven, que es su antecesor directo. Vemos cómo se definen estos valores en el archivo `build.gradle.kts`:

```kotlin
group = "ar.edu.unsam.algo2"
version = "1.0-SNAPSHOT"
```

El artifact se define en el archivo `settings.gradle`:

```kotlin
rootProject.name = "eg-seguros-kotlin"
```

## Repositorios locales

Las **dependencias** son útiles para poder acceder a una biblioteca que resuelva una determinada funcionalidad (como [Mockk](https://mockk.io/) o [Apache Commons Collections](https://commons.apache.org/proper/commons-collections/)). Para facilitar la descarga de esa dependencia, se suben como **artefacto** con diferentes números de versión a un **repositorio**. Cuando instalamos Gradle, se crea un repositorio local en una carpeta que por defecto suele ser `HOME/.gradle/caches/modules-xxx`. Si queremos ubicar al componente cuyo identificador es `io.kotest:kotest-assertions-core:5.1.0` podremos encontrarlo localmente en

```bash
~/.gradle
   └──caches
      └── modules-2
            └── files-2.1
                    └── io.kotest
                          └── kotest-assertions-core
                                ├─ 4.4.3 (otra versión)
                                └─ 5.1.0 --> dentro de esta carpeta estará el artefacto
```

- recordemos que el identificador de un componente se arma a partir del groupId + el artifactId + la versión
- en la cache de Gradle están todos los componentes que descargamos localmente. Esto permite que cuando estemos trabajando en otro proyecto que comparta la misma dependencia no necesitemos ir a descargarla desde los repositorios. El comportamiento en una máquina Windows es exactamente igual, hay que explorar los directorios incluyendo los que son ocultos, y navegar a partir de la carpeta de usuario\.gradle.

## Repositorios remotos

Ahora bien, ¿desde dónde descargamos las versiones 4.4.3 y 5.1.0 de io.kotest:kotest-assertions-core? Existen para eso **repositorios remotos** donde se publican artefactos:

- **Maven Central**, que sigue siendo el repositorio principal donde están subidos artefactos publicados con tecnología [Maven](https://maven.apache.org/). Las dependencias más importantes suelen estar en este repositorio y tenés una [página de búsqueda de artefactos](https://search.maven.org/), muy útil cuando necesitamos bajarnos "Mockito", "Log4J", "Kotest" o cualquier otra dependencia.
- **Google**: para los proyectos en Kotlin, otro repositorio importante es el de [Google (también en Maven)](https://maven.google.com/web/index.html) ya que contiene componentes relacionados con el desarrollo de Android, entre otros.
- hay eventualmente otros repositorios remotos e incluso podés crear un servidor que funcione como repositorio de artefactos (podés investigar [Artifactory | JFrog](https://jfrog.com/artifactory/)), por el momento es suficiente con saber simplemente desde dónde estamos descargando nuestros componentes.

En el archivo `build.gradle.kts` podés ver cómo se referencian los repositorios remotos:

```kotlin
repositories {
    mavenCentral()
}
```

### Definiendo dependencias en el proyecto

Las dependencias se definen dentro de un tag `dependencies`:

```kotlin
val kotestVersion = "5.1.0"

dependencies {
    implementation(kotlin("stdlib"))
    testImplementation("io.kotest:kotest-runner-junit5:$kotestVersion")
    testImplementation("io.kotest:kotest-assertions-core:$kotestVersion")
}
```

En este caso, estamos definiendo que nuestro proyecto tiene como pre-requisitos:
- la biblioteca estándar de Kotlin
- y los componentes `kotest-runner-junit5` y `kotest-assertions-core` de `io.kotest`. La versión se define en una referencia `val`, el código que escribimos para definir el archivo de configuración es Kotlin.

Una vez que hagamos un cambio en las dependencias, nos aparece el botón para sincronizar las definiciones del archivo con el IntelliJ, como vemos en este video:

![descarga componente local](/img/wiki/gradle-add-dependency.gif)

Al agregar una dependencia lo hacemos con el formato "group:artifact:version" y también definimos **el tipo de alcance** (scope), que puede ser

- **implementation**: el componente es necesario para hacer el _build_ de nuestro proyecto.
- **testImplementation**: el componente es necesario para ejecutar los tests de nuestro proyecto.
- **runtimeOnly**: el componente se utiliza únicamente cuando tenemos levantada la aplicación. Esta variante es útil cuando queremos trabajar con objetos de resguardo (_stubs_ o _mocks_) para ejecutar los tests pero queremos tener un componente real que envía mails en la versión productiva.
- otras variantes pueden estudiarse leyendo [la documentación del plugin de Gradle para Java](https://docs.gradle.org/current/userguide/java_library_plugin.html#sec:java_library_configurations_graph)
- **_custom_**: para les interesades dejamos un artículo que explica [cómo definir configuraciones propias extendiendo el modelo de Gradle](https://docs.gradle.org/current/userguide/declaring_dependencies.html)

### Resumen general del manejo de dependencias de Gradle

![manejo de dependencias de gradle](/img/wiki/gradle-dependencias.png)

_Fuente:_ https://docs.gradle.org/current/userguide/dependency_management.html

Al hacer el build de nuestro proyecto

- si la versión de ese componente se encuentra descargado en nuestro repositorio local, se utiliza como dependencia válida
- en caso contrario habrá que buscar en los repositorios definidos en nuestro proyecto, como Maven Central, google, etc. en el orden en que fueron definidos
- si en ninguno de los repositorios fue posible encontrarlo, entonces recibiremos un mensaje de error cuando intentemos hacer build del proyecto:

![gradle componente fallido](/img/wiki/gradle-componenteFallido.png)

## Plugins

Si bien Gradle provee una plataforma para poder facilitar el manejo de dependencias, la compilación tanto de nuestros objetos de dominio como los tests y muchas otras actividades más, quienes verdaderamente se encargan de esta tarea son los **plugins**, que son la implementación final que resuelve toda esta magia.

Cada plugin permite

- definir qué elementos van a participar, extendiendo el modelo original de Gradle: por ejemplo el plugin de Kotlin define que el directorio donde se ubican las clases principales es `src/main/kotlin`
- crear tareas, como la compilación de todos los archivos Kotlin, o la ejecución de los tests unitarios indicando cuál es el framework, entre muchas otras
- agregar configuraciones a nuestro proyecto, como repositorios adicionales donde ir a buscar dependencias, o definir una versión de la JDK por defecto.

Podés ver en cualquiera de nuestros ejemplos qué contiene la sección plugins del `build.gradle.kts`:

```kotlin
plugins {
    kotlin("jvm") version "1.6.10"
    jacoco
}
```

- estamos utilizando una determinada versión del plugin de Kotlin para la JVM, para poder trabajar el proyecto adecuadamente en esa tecnología
- y además se agrega JaCoCo (Java Code Coverage), que agrega tareas para poder escribir reportes de % de cobertura de nuestros tests que luego serán utilizados por sitios que los publican, como [Codecov](https://about.codecov.io/) o [Coveralls](https://coveralls.io/)

<br/>
Para más información pueden consultar [esta página](https://docs.gradle.org/current/userguide/plugins.html).

## Dependencias transitivas

Un detalle no menor de la resolución de dependencias de Gradle es que también funciona para las dependencias transitivas.

Por ejemplo:

- proyectoA --> proyectoB
- proyectoB --> proyectoC
- proyectoC --> proyectoD, proyectoE, proyectoF

Al resolver las dependencias, el proyectoA necesitará descargar los componentes B, C, D, E y F. Incluso podríamos requerir diferentes versiones de los mismos componentes.

<br/>
Noten que un proyecto comercial "normal" o mediano, puede incluir decenas y hasta cientos de dependencias. Esto se puede ver en IntelliJ desde la solapa Gradle > Test Compile Path...

![Gradle - Dependencias transitivas](/img/wiki/gradle-transitive-dependencies.gif)

## Tareas de Gradle (TODO)

Mostrar ejecución en IntelliJ y qué representan.

## Gradle Wrapper (TODO)



## Ejecutando gradle desde la consola (TODO)

Una alternativa es trabajar directamente con Gradle desde la consola, algo que puede ser útil para automatizar tareas, como cuando trabajemos con herramientas de integración continua.

```bash
mvn clean compile
```

Esto ejecuta varios plugins en forma sincronizada:

- por un lado borra los directorios de destino, como hemos contado en el párrafo plugins
- y luego compila los fuentes del proyecto, lo cual implica descargarse las dependencias, y en el caso del plugin de Xtend, convertir los archivos `.xtend` a `.java` (y luego generar los `.class`)

Si queremos ver el árbol de dependencias transitivas, podemos escribir

```bash
mvn dependency:tree
```

Pero ¿cómo sabemos qué comando debemos ejecutar? Para eso hay que entender el ciclo de vida de un build de Maven.

## Documentación oficial

Para más información recomendamos leer 

- la [documentación oficial del proyecto Gradle](https://docs.gradle.org/current/userguide/userguide.html)
- tenés también una [página de tutoriales de Gradle](https://gradle.org/guides/)
- un video bastante exhaustivo de personas que trabajan en Gradle: [Getting Started with the Gradle DSL Kotlin](https://www.youtube.com/watch?v=KN-_q3ss4l0) de Paul Merlin & Rodrigo de Oliveira.
- y un [tutorial sobre Kotlin DSL](https://www.youtube.com/watch?v=zYNbsVv9oN0) de Anton Arhipov

## Links relacionados

- [Página principal de Algoritmos 2](algo2-temario.html)
