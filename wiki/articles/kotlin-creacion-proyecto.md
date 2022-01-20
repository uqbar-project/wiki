---
layout: article
title: Kotlin - Cómo generar un proyecto desde cero
categories: [xtend, nuevo proyecto, maven]
featured: true
---

Para realizar las prácticas, vas a crear un proyecto desde cero. Como ahora hay muchas más opciones, te dejamos una guía simple de cómo iniciarte.

# Crear proyecto Kotlin

Desde IntelliJ tenemos dos opciones:

- sin ningún proyecto abierto, nos aparece un botón `New project`
- o bien si tenemos un proyecto abierto, tenemos que abrir el menú `File > New > Project...`

Eso abre la siguiente ventana de diálogo:

![image](/img/wiki/kotlin-nuevo-proyecto.png)

- el nombre del proyecto no debe contener espacios ni caracteres especiales (IntelliJ te va a avisar)
- Todos los ejemplos que vas a descargar de la materia, así como los proyectos en los que vas a trabajar, se basan en la tecnología **Gradle**. Asegurate que tengas seleccionada la opción `Gradle Kotlin`.
- Revisamos que la opción en `Project JDK` por defecto sea la **JDK 14**, en caso contrario debemos ir a [configurar la versión por defecto de Java](./kotlin-preparacion-de-un-entorno-de-desarrollo.html)
- Te recomendamos que el group id sea `ar.edu.zzzz.xxx` donde `zzzz` sea la universidad y `xxx` sea la materia que estás cursando. Por ejemplo `ar.edu.unsam.algo2` para la materia Algoritmos 2 de UNSAM.
- El nombre del artefacto (Artifact ID) es el nombre de tu proyecto

Cuando finalizamos, se genera un proyecto con un archivo `build.gradle.kts`, que es fundamental para que Eclipse IntelliJ el proyecto en otra máquina y descargue las dependencias

## Archivo de configuración de Gradle

Te dejamos entonces un archivo con las dependencias base para la cursada de Algoritmos 2 (UNSAM) del aǹo 2022: [build.gradle.kts de ejemplo](algo2.build.gradle.kts). Luego tendrás que

- renombrar el archivo a `build.gradle.kts`
- copiarlo dentro del directorio raíz de tu proyecto ya creado
- revisar el _groupId_ para ver si es el adecuado
- revisar las dependencias

Una vez que actualicemos ese archivo, desde IntelliJ nos aparecerán dos íconos para indicarnos que debemos sincronizar las definiciones de Gradle con las de nuestro IDE:

![image](/img/languages/kotlin-actualizar-gradle.gif)

Al hacer click automáticamente se actualizarán las dependencias. Este proceso es muy importante ya que de otra manera podremos experimentar problemas como imports que no funcionan, o métodos inexistentes (por estar usando versiones diferentes a las que queremos realmente).


## Continuous integration

Otra herramienta que vamos a incorporar en la cursada es la integración continua, que consiste en validar cada porción de código o **incremento** que subamos, de la siguiente manera

- descargamos la última versión del repositorio en una máquina en la nube
- se hace el build del proyecto: se descargan las dependencias, se compilan los archivos fuentes, etc.
- se ejecutan los tests y todos deben pasar

Esto permite detectar inconsistencias entre diferentes estaciones de trabajo, o errores más básicos como "algo no compila" o "estos 3 tests se rompieron".

Por el momento, lo que necesitás es únicamente copiar [este archivo](./algo2.build.yml) al nuevo directorio `.github/workflows` de tu proyecto, con el nombre `build.yml`. No hace falta configurar nada más.


## Primeros pasos

Vamos a crear nuestra primera clase Perro. Es importante notar que tendremos dos carpetas donde ubicaremos los fuentes:

- `src/main/kotlin`: donde irán las clases
- `src/test/kotlin`: donde irán los tests

Por eso, nos ubicamos en `src/main/kotlin` y con un botón derecho, `New > Kotlin Class/File`.

![image](/img/wiki/kotlin-nueva-clase.gif)


## Shortcuts de IntelliJ

A continuación te dejamos algunas recomendaciones para que tu estadía en IntelliJ + Kotlin sea más feliz:

- "Cómo era para...?" Lo mejor es preguntarle al propio IDE, **presionando dos veces `Shift`, `Shift`**

![image](/img/wiki/intellij-shiftShift.gif)

- Presionar dos veces `Ctrl` + `Ctrl` te permite ejecutar cualquier comando válido desde el componente donde estés ubicado.

![image](/img/wiki/intellij-ctrlCtrl.gif)

- `Alt` + `Enter` activa sugerencias tanto para errores como para cosas que se pueden mejorar (_warnings_)

- Nunca nos olvidemos de que nuestro código tiene que ser entendible para el resto de la humanidad y lo mejor es pedirle al IDE que lo haga mediante `Ctrl` + `Alt` + `L`. 

Cómo configurarlo.

![image](/img/languages/formattingCode.gif)

Para más información podés ver [este artículo](https://blog.jetbrains.com/idea/2020/03/top-15-intellij-idea-shortcuts/).

## Packages para agrupar código común

- Utilización de packages (paquetes). Es una buena práctica agrupar las clases afines en paquetes para organizar semánticamente el código. No hay una guía firme a seguir con respecto a cómo organizar nuestro código, ya que suele depender del contexto en el cual estamos trabajando, pero a medida que veas nuestros ejemplos y vayas haciendo las prácticas notarás que hay clases que se pueden agrupar en contextos similares. Te dejamos un ejemplo

```bash
proyecto
   ├── home
   ├── registration
   │   ├── Profile.xtend
   │   └── User.xtend
   └── settings
       ├── CustomPrivacy.xtend
       ├── DefaultPrivacy.xtend
       ├── Privacy.xtend
       └── Setting.xtend
```

De esta manera, logramos mayor granularidad en la organización de nuestras clases.
____

# Links útiles

* [Siguiente paso: Sobre el control de versiones](kotlin-amigandonos-git.html)
* [Volver al menú principal del entorno Kotlin](kotlin-principal.html)
