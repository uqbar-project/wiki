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

- la comunicación entre las personas que desarrollan en el proyecto (es importante que se hablen entre ustedes)
- un servidor de integración continua: Github Actions, CircleCI, Jenkins, etc.
- configurar nuestro proyecto contra ese servidor: eso puede involucrar uno o más archivos
- una herramienta de versionado: git, Mercurial, etc.
- y sobre todo, **una buena calidad de tests automatizados**

## Estrategia propuesta para proyectos con Kotlin

A continuación vamos a describir los pasos necesarios para que tu proyecto Kotlin tenga integración continua

### Servidor de integración continua

El servidor de integración continua será el que provee Github, y tiene las ventajas de estar integrado a tu repositorio y no tener que hacer nada para activarlo. Podés navegarlo en la solapa Actions:

<br>
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

A partir de aquí, cada vez que

* hagas un push en las branches "main" o "master"
* crees un PR que quiera mergear a "main" o "master" o hagas un push a esa rama

[Github Actions](https://docs.github.com/es/actions/learn-github-actions/understanding-github-actions) automáticamente hará lo siguiente:

- clonará tu repositorio
- lo compilará (_build_) en Kotlin mediante el script de Gradle
- ejecutará los tests
- en caso de error, mandará un mail avisando que el build falló (por el momento solo al autor del commit/PR)
  - si anduvo ok, por defecto no recibirás ninguna notificación
- si es un push directo a la rama principal, actualizará la badge de cobertura de JaCoCo en `.github/badges/jacoco.svg`
- si es un pull request comentará la cobertura actual luego de aplicar los cambios sugeridos (lo vas a ver en la página del PR mismo)
- finalmente, subirá a la descripción de esta instancia del action un "artefacto" con un reporte de cobertura generado por JaCoCo en HTML
  - los artefactos son archivos que github permite almacenar, junto a logs, junto a un intento de build durante un periodo determinado de tiempo (actualmente un máximo de 90 días, tras lo cual son eliminados)

La principal ventaja: al automatizarlo no tendremos que acordarnos de hacerlo. Queda además registrado si cada commit pasa o no, lo cual nos ayuda a encontrar dónde se originó un error en el código.

### Agregando el Badge de Build al README

El _badge_ es un indicador visual de cómo resultó el último build, que ubicaremos en el archivo README. Para eso, 

- ingresamos a nuestro repositorio, solapa Actions, 
- elegimos cualquiera de los builds 
- luego a la derecha hacemos click sobre el botón que tiene los tres puntos: `...`
- en el menú contextual elegimos la opción "Create Status Badge", elegimos la rama que queremos y
- presionamos el botón de copia. 

<br>
<!-- -->

![Kotlin CI - Crear status badge](/img/wiki/kotlin-ci-gh-actions-badge.gif)

<!-- -->
<br>

- luego vamos al editor, pegamos el texto en el README y pusheamos al repositorio.

# Links útiles

* [Video en youtube que explica cómo es la integración continua con Kotlin y Github Actions](https://youtu.be/B6W9QzOutkc)
* [Volver al menú principal del entorno Kotlin](kotlin-principal.html)
