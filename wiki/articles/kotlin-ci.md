---
layout: article
title: Integración continua para materias con Kotlin
categories: [github actions, ci, integracion, continua]
featured: true
---

# Integración continua (CI)

## Definición

La integración continua busca automatizar los cambios en el código que escriben varias personas en un proyecto de software. De esa manera

- facilita la experiencia de descargarse un proyecto y hacerlo funcionar localmente en nuestras máquinas
- permite la detección temprana de errores, cuando es más fácil resolverlos.

## Qué necesitamos para implementarla en nuestro proyecto

Para poder implementar la integración continua, necesitamos

- un servidor de integración continua: Github Actions, CircleCI, Jenkins, etc.
- configurar nuestro proyecto contra ese servidor: eso puede involucrar uno o más archivos
- la comunicación de las personas que desarrollan en el proyecto
- una herramienta de versionado: git, Mercurial, etc.
- y sobre todo, **una buena calidad de tests automatizados**

## Estrategia propuesta para proyectos con Kotlin

A continuación vamos a describir los pasos necesarios para que tu proyecto Kotlin tenga integración continua

### Servidor de integración continua

El servidor de integración continua será el que provee Github, y tiene las ventajas de estar integrado a tu repositorio y no tener que hacer nada para activarlo. Podés navegarlo en la solapa Actions:

<!-- -->

![GH Actions - repo](/img/wiki/kotlin-ci-gh-actions-repo.gif)

<!-- -->

### Configuración del CI en el proyecto

Copiate [este archivo](./algo2.build.yml) en la siguiente estructura que **hay que crear**

```bash
<directorio raíz>
└── .github
    └── workflows
        └── build.yml
```

### Qué pasa entonces

A partir de aquí, cada vez que hagas un push, [Github Actions](https://www.travis-ci.com/) como servidor de integración continua

- clonará el repositorio tuyo
- lo compilará (_build_) en Kotlin mediante un script de Gradle
- ejecutará los tests
- y generará el nuevo status de tu proyecto
- en caso de error, te mandará un mail avisándote que el build falló (por el momento solo al autor del commit)
- y si anduvo ok, por defecto no recibirás ninguna notificación
- de aquí en más, cada vez que hagas un push al repositorio, si anda ok no te dirá nada, pero si falla, te mandará un mail. Es mejor saberlo antes, ¿no? porque además queda registrado cada commit si pasa o no, y eso ayuda a encontrar cuál es el código que "rompió todo".

### Agregando el Badge al README

El _badge_ es un indicador visual de cómo resultó el último build, que ubicaremos en el archivo README. Para eso, 

- ingresamos a nuestro repositorio, solapa Actions, 
- elegimos cualquiera de los builds 
- luego a la derecha hacemos click sobre el botón que tiene los tres puntos: `...`
- en el menú contextual elegimos la opción "Create Status Badge", elegimos la rama que queremos y
- finalmente presionamos el botón de copia. 

<!-- -->

![Kotlin CI - Crear status badge](/img/wiki/kotlin-ci-gh-actions-badge.gif)

<!-- -->

Vamos al editor, pegamos el texto en el README y pusheamos al repositorio.

# Links útiles

* [Volver al menú principal del entorno Kotlin](kotlin-principal.html)
