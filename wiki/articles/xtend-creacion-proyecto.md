---
layout: article
title: Cómo generar un proyecto Xtend nuevo
categories: [xtend, nuevo proyecto, maven]
featured: true
---

Para realizar las prácticas, vas a crear un proyecto desde cero. Como ahora hay muchas más opciones, te dejamos una guía simple de cómo iniciarte.

# Crear proyecto Maven

Todos los ejemplos que vas a descargar de la materia, así como los proyectos en los que vas a trabajar, se basan en la tecnología Maven. Para crear un proyecto Maven, te dejamos esta animación:

![image](/img/languages/creatingNewProject.gif)

- Seleccionamos New > Project... Maven Project
- Chequeamos "Skip archetype selection" ya que no lo usaremos, y luego Next
- En la siguiente pantalla, escribimos cualquier cosa en "groupId", que es un valor que luego borraremos, pero que si lo dejamos vacío Eclipse nos mostrará un error. Y el nombre de nuestro proyecto o `artifactId`, éste sí es importante, seguimos la nomenclatura de paquetes de Java, para que no haya confusiones en los nombres: `ar.edu.unsam.prueba` identifica nuestro proyecto en todo el mundo
- cuando finalizamos, se genera un proyecto con un archivo pom.xml, que es fundamental para que Eclipse regenere el proyecto en otra máquina y descargue las dependencias

# Actualizando el parent project

- si descargamos previamente algún otro proyecto, podemos copiar el **parent project**, o bien podés copiarlo aquí

```xml
<parent>
    <groupId>org.uqbar-project</groupId>
    <artifactId>uqbar-xtend-parent</artifactId>
    <version>2.17.1</version>
</parent>
```

El parent project permite reutilizar definiciones comunes entre varios proyectos. En nuestro caso, uqbar-xtend-parent (el nombre que le dimos a este artefacto) sirve para definir

- que utilizaremos la versión 2.17.0 de Xtend
- con una dependencia para correr tests unitarios
- compilando a JDK 1.8
- y muchas otras cosas más que iremos viendo poco a poco

Lo podés encontrar buscando en esta URL: https://search.maven.org/, más concretamente https://search.maven.org/search?q=uqbar-xtend. Es un componente que hemos puesto en la nube de los repositorios de Maven para simplificarle un poco la tarea inicial a ustedes.

# Sincronización entre Eclipse y Maven

Cada vez que hagamos un cambio en el archivo `pom.xml`, nos aparecerá un mensaje de error en la solapa **Problems**, que se soluciona forzando la sincronización entre Eclipse y Maven (dado que cada uno maneja su propia estructura de proyectos Java). Como regla general, **siempre que necesitemos agregar alguna biblioteca, o dependencia, debemos hacerlo en el archivo pom y no desde las opciones que ofrece el Eclipse, porque nuestros compañeros o los docentes no tendrán esa biblioteca**. Para sincronizar Maven y Eclipse, nos paramos en el proyecto y con un botón derecho elegimos "Maven > Update Project".

# Primeros pasos

![image](/img/languages/firstClass.gif)

Ahora solo nos queda eliminar la línea groupId (con Ctrl + D), formatear el pom.xml (con Ctrl + Shift + F) y crear nuestra primera clase Perro. Es importante notar que tendremos dos carpetas donde ubicaremos los fuentes:

- `src/main/java`: donde irán las clases
- `src/test/java`: donde irán los tests

Por eso, nos ubicamos en `src/main/java` y con un botón derecho, New > Xtend Class (es importante que hayas configurado el Eclipse para que no esté escondida esta opción). 

# Recomendaciones

A continuación te dejamos algunas recomendaciones para que tu estadía en Eclipse + Xtend sea más feliz:

- ¡Formatear el código! Nunca nos olvidemos de que nuestro código tiene que ser entendible para el resto de la humanidad. Además, el Eclipse lo hace solo (Ctrl + Shift + F).

![image](/img/languages/formattingCode.gif)

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

* [Sobre el control de versiones](xtend-amigandonos-git.html)
* [Volver al menú principal del entorno Xtend](xtend-principal.html)