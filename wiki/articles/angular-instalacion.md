---
layout: article
title: Instalacion de Entorno Angular
featured: true
---

![angular](/img/languages/angular1.png)

# Entorno

Es necesario que instales las siguientes herramientas, en este orden:

- Si estás en entorno Windows te recomendamos instalarte [Git Bash](https://gitforwindows.org/)
- Seguimos con [NodeJS](https://nodejs.org/en/), preferentemente la última versión.
- Luego [NPM (Node Package Manager)](https://www.npmjs.com/), con el que vamos a hacer los builds de nuestras aplicaciones.
- El [Angular CLI (Command line interface)](https://github.com/angular/angular-cli/blob/master/packages/angular/cli/README.md) se instala con npm:

```bash
npm install -g @angular/cli
```

- El editor de texto que vamos a soportar en la cursada es [**Visual Studio Code**](https://code.visualstudio.com/) (hay [una versión portable](https://sourceforge.net/projects/vscode-portable/) si estás en una máquina sin privilegios de administrador). 

Otra opción es utilizar [Web Storm](https://www.jetbrains.com/webstorm/) (de la suite de IntelliJ), si tienen una cuenta de la facultad pueden solicitar una licencia educativa. Solo que como no vamos a aprovechar todas las herramientas de este IDE poderoso quizás convenga ir por el Visual Studio Code.

- Los plugins del Visual Studio Code que te recomendamos al 2018 son:
  - **TypeScript Hero**: herramientas generales para programar en Typescript
  - **Angular 6 Snippets - TypeScript, Html, Angular Material, ngRx, RxJS & Flex Layout**: autocompletado de templates de Angular
  - **Angular Files**: permite generar desde el menú de visual studio code componentes, pipes, servicios, etc.
  - y como chiche extra: **VSCode simpler Icons with Angular** para tener íconos diferentes (hay que seguir los pasos de instalación de la página del plugin)

# Aprendiendo Typescript

Typescript es el lenguaje de programación base para Angular. Tranquilo, es muy similar a los lenguajes orientados a objetos en los que ya trabajaste. Para iniciarte o para hacer consultas te dejamos estos links:

- [Documentación oficial de Typescript](https://www.typescriptlang.org/docs/home.html): tiene una intro de 5 minutos, otros tutoriales cortos y el Handbook para sacarse dudas
- [Aprendiendo Typescript en 30 minutos](https://tutorialzine.com/2016/07/learn-typescript-in-30-minutes): muy buen tutorial para comenzar explicando los conceptos más salientes
- [Tutorial de Typescript en castellano y PDF](https://www.gitbook.com/download/pdf/book/khru/typescript): material de consulta en castellano para los interesados

# Crear un proyecto Angular desde cero

En la consola Git Bash o bien desde una terminal de Linux hacemos

```bash
ng new nombre-de-tu-app
cd nombre-de-tu-app
ng serve -open  # o bien, la versión corta es ng s -o
```

# Correr los tests de un proyecto

Para ejecutar los tests de un proyecto, te posicionás en el directorio raíz y ejecutás desde la consola

```bash
ng test --watch --sourceMap=false
```
