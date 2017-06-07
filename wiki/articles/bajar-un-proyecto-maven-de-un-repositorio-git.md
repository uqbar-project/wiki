---
layout: article
title: Bajar un proyecto maven de un repositorio git
---

# Descripción

Este artículo asume la presencia de un entorno de trabajo con una JDK. En caso de no contar con un repositorio de esas características conviene leer las instrucciones para la [preparación de un entorno de desarrollo Java](preparacion-de-un-entorno-de-desarrollo-java.html).

También se asume la preexistencia de un proyecto mavenizado y publicado en un repositorio Git, si lo que se desea es crear el proyecto en lugar de descargarlo, aquí están las instrucciones para la [creación de un proyecto maven básico](creacion-de-un-proyecto-maven-basico.html) y su posterior publicación en el repositorio.

El proceso tiene los siguientes pasos, que se detallan a continuación:

![Git](/img/languages/git_clone3.png)

-   Clonar el proyecto desde el repositorio remoto y alojarlo en nuestro espacio de trabajo local.
-   Adaptar el proyecto maven para ser utilizado dentro del entorno Eclipse.

# Descarga (clone) 

El checkout se puede hacer 

- desde el eclipse 
- desde un cliente git 
- o por consola

A continuación explicaremos los tres pasos por separado.


## Por línea de comando

Para esto debemos 

- ubicarnos en el directorio de trabajo
- saber la URL del repositorio en el que se publicó el proyecto

<!-- -->

```bash
$ cd ~/workspace/materia
$ git clone https://github.com/uqbar-project/eg-vehiculos-xtend
```

En el directorio eg-vehiculos-xtend se bajan los recursos del proyecto, incluyendo un directorio .git donde está la información. De ser necesario debemos cambiar la rama o branch de trabajo, por ejemplo al branch *dev*:

```bash
$ git checkout dev
```

<!-- -->

## Descarga desde el Eclipse

En caso de hacerlo desde el eclipse, la forma de hacerlo es:

-   Copiar en el portapapeles la URL del repositorio al que queremos apuntar
-   Ir a la perspectiva "Git"
-   En la solapa "Git Repositories" hacer click derecho sobre algún espacio en blanco y ahí elegir la opción "Paste Repository Path or URI", luego botón "Next"...

![Step 1 Git clone](/img/languages/git_clone_step1.png)

![Step 2 Git clone](/img/languages/git_clone_step2.png)

-   Elegir una rama o branch para descargar (master por defecto), luego botón Next...

![Step 3 Git clone](/img/languages/git_clone_step3.png)

-   Seleccionar la carpeta del destino

![Step 4 Git clone](/img/languages/git_clone_step4.png)

-   Chequear la opción "Import all Eclipse projects after clone finishes"



## Descarga de un proyecto desde un cliente git

Eso puede variar dependiendo del cliente, te dejamos algunos links

-   [Smartgit](http://www.syntevo.com/doc/display/SG/Check+Out)
-   [Source Tree](https://confluence.atlassian.com/sourcetreekb/clone-a-repository-into-sourcetree-780870050.html)
-   [Git Kraken](https://support.gitkraken.com/repositories/remote)


### Adaptar un proyecto maven para ser usado desde el Eclipse

Si importaste el proyecto desde la consola o bien el cliente solo descargó el proyecto en un espacio de trabajo local, lo que faltaría hacer es File > Import... > Existing Maven projects...

Luego de posicionarse en el directorio donde descargamos el proyecto, en particular donde se encuentra el archivo pom.xml, seleccionamos dicho proyecto y luego presionamos el botón "Finish". 

Entonces nuestro proyecto toma la definición del pom.xml y se construye para ser usado en el IDE Eclipse. Vemos que la naturaleza del proyecto es Maven, porque tiene una M arriba del ícono del proyecto:

![proyecto maven](/img/languages/project_maven.png)

¡Ya podemos comenzar a trabajar!


