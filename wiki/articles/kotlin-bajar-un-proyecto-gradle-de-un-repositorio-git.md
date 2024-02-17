---
layout: article
title: Bajar un proyecto Kotlin - Gradle de un repositorio git
---

# Descripción

Este artículo asume la presencia de un entorno de trabajo Kotlin. En caso de que todavía no tengas instaladas esas herramientas, te recomendamos leer [estas instrucciones](kotlin-preparacion-de-un-entorno-de-desarrollo.html).

También se asume la preexistencia de un proyecto construido con Gradle y publicado en un repositorio Git, si lo que se desea es crear el proyecto en lugar de descargarlo, aquí están las instrucciones para la [creación de un proyecto](kotlin-creacion-proyecto.html) y su posterior publicación en el repositorio.

El proceso tiene los siguientes pasos, que se detallan a continuación:

![Git](/img/languages/git_clone3.png)

- Clonar el proyecto desde el repositorio remoto y alojarlo en nuestro espacio de trabajo local.
- Hacer el build del proyecto Kotlin en IntelliJ tomando como base las definiciones de Gradle.

# Descarga (clone)

El checkout se puede hacer 

- desde el IntelliJ
- o por consola

A continuación explicaremos ambos pasos por separado.


## Por línea de comando

### Paso 1: clonación

Para esto debemos 

- ubicarnos en el directorio de trabajo
- saber la URL del repositorio en el que se publicó el proyecto

<!-- -->

Por ejemplo:

```bash
# buscá el directorio principal donde estén tus proyectos
$ cd ~/workspace/algo2

# clonamos la URL
$ git clone https://github.com/uqbar-project/eg-seguros-kotlin

# en la carpeta eg-seguros-kotlin se baja el proyecto
```

En el directorio local (en este caso `eg-seguros-kotlin`) se bajan los recursos del proyecto, incluyendo un directorio `.git` donde está la información. De ser necesario debemos cambiar la rama o branch de trabajo, por ejemplo al branch *dev*:

```bash
$ git checkout dev
```

### Paso 2: Importación del proyecto en IntelliJ

Para importar un proyecto en IntelliJ una vez descargado

- si tenés un proyecto abierto, desde el menú principal: "File" > "Open..."
- si no hay ningún proyecto abierto, el botón "Open"

Nos puede aparecer una ventana de diálogo para que confiemos en el proyecto:

![IntelliJ confiar en el proyecto](/img/wiki/importProject_10.png)

Aceptamos seleccionando la opción "Trust project" y entonces se importará el proyecto, al detectar que está hecho en Gradle se utilizará el archivo correspondiente para hacer el build. Te recomendamos que actives el check para que IntelliJ confíe en todos los proyectos que te descargás en la carpeta raíz de la materia.

![IntelliJ build del proyecto](/img/wiki/importProject_11.png)

¡Y ya podemos comenzar a trabajar!


<!-- -->

## Descarga desde IntelliJ

### Integración de tu usuario de github (solo la primera vez)

En caso de utilizar directamente el plugin de IntelliJ, te recomendamos integrar tu usuario de Github de la siguiente manera

- si tenés un proyecto abierto, desde el menú principal: "Git" > "Clone..."
- si no hay ningún proyecto abierto, el botón "Get from VCS"

Aparece esta ventana de diálogo, seleccionamos Github:

![Github - selección en IntelliJ](/img/wiki/importProject_02.png)

___

Se abre una ventana de un navegador donde nos pide autorización para acceder a nuestra cuenta de github:

![Github - autorización](/img/wiki/importProject_03.png)

<!-- -->

___

Confirmamos qué cuenta de Github es la que vamos a integrar

![Github - selección de la cuenta 1](/img/wiki/importProject_04.png)

y presionamos el botón "Authorize JetBrains":

![Github - selección de la cuenta 2](/img/wiki/importProject_06.png)


<!-- -->
___

Ingresamos la contraseña

![Github - password](/img/wiki/importProject_07.png)

<!-- -->
___

(si activaste la autenticación en 2 pasos o 2FA es probable que tengas que ingresar tu token también). 

Una vez finalizado este paso ya podemos cerrar el navegador y volver a IntelliJ:

![Github - cerrar navegador](/img/wiki/importProject_08.png)

<!-- -->

### Clonar un proyecto en IntelliJ

Una vez que tengamos asociado el usuario el proceso es muy sencillo, porque tendremos acceso a todos los repositorios 

![IntelliJ clonar proyecto](/img/wiki/importProject_09.png)

Aquí podemos hacer una búsqueda y nos aparecerán todos los repositorios a los que tenemos acceso. Por último, solo debemos seleccionar el directorio donde vamos a bajar localmente nuestro proyecto para trabajar con el IDE y presionar el botón "Clone". Nos puede aparecer una ventana de diálogo para que confiemos en el proyecto:

![IntelliJ confiar en el proyecto](/img/wiki/importProject_10.png)

Aceptamos seleccionando la opción "Trust project" y entonces se importará el proyecto, al detectar que está hecho en Gradle se utilizará el archivo correspondiente para hacer el build. Te recomendamos que actives el check para que IntelliJ confíe en todos los proyectos que te descargás en la carpeta raíz de la materia.

![IntelliJ build del proyecto](/img/wiki/importProject_11.png)

Si necesitamos movernos a otra rama, eso se puede hacer mediante la opción "Git" > "Branches" y seleccionando la rama que quieras + Checkout. También tenés la opción de hacer click sobre la rama que se muestra en la parte inferior del IDE:

![IntelliJ cambiar la rama de Git](/img/wiki/intellij_changingBranch.gif)

¡Y ya podemos comenzar a trabajar!

<!-- -->

# Links útiles

* [Cómo trabajar con el control de versiones](kotlin-amigandonos-git.html)
* [Cómo crear un proyecto Kotlin con Gradle](kotlin-creacion-proyecto.html)
* [Volver al menú principal del entorno Kotlin](kotlin-principal.html)
