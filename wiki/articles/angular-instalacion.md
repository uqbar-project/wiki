---
layout: article
title: Instalacion de Entorno Angular
featured: true
categories: [web, angular, ui, configuracion, entorno, client-side]
---

![angular](/img/languages/angular1.png)

# Entorno

Es necesario que instales las siguientes herramientas, en este orden:

- Si estás en entorno Windows te recomendamos instalarte [Git Bash](https://gitforwindows.org/)
- Seguimos con [NodeJS](https://nodejs.org/en/).
  - Si estás en entorno Linux/Mac recomendamos que descargues Node desde [nvm (Node Version Manager)](https://github.com/nvm-sh/nvm) y luego instales esta versión: `nvm install 20.4.0`
  - Si estás en Windows instalate la versión actual
- Luego [NPM (Node Package Manager)](https://www.npmjs.com/), con el que vamos a hacer los builds de nuestras aplicaciones.
  - Para familiarizarte con el manejo de dependencias, te dejamos [este artículo](npm-dependencias.html)  
- El [Angular CLI (Command line interface)](https://github.com/angular/angular-cli/blob/master/packages/angular/cli/README.md) se instala con npm:

```bash
npm install -g @angular/cli
```

# Editor de Texto

## Visual Studio Code

- El editor de texto que vamos a soportar en la cursada es [**Visual Studio Code**](https://code.visualstudio.com/) (hay [una versión portable](https://sourceforge.net/projects/vscode-portable/) si estás en una máquina sin privilegios de administrador).

Dentro de Visual Studio Code, te recomendamos que crees un perfil vacío y lo asocies a tus nuevos proyectos Angular (podés ver [cómo se trabaja con perfiles en VSCode en este video](https://www.youtube.com/watch?v=_2F2Zt-_tUA), la recomendación es partir con un profile totalmente vacío. También te dejamos [este tutorial muy piola - en inglés](https://www.youtube.com/watch?v=QjvvqR9KyVo) y [la documentación oficial](https://code.visualstudio.com/docs/editor/profiles)). Las extensiones que recomendamos para trabajar son:

Los plugins del Visual Studio Code que te recomendamos al 2024 son los que ya instalaste para trabajar con HTML/CSS y los siguientes:

### Necesarios ###

- **Angular Language Service (Angular)**: autocompletado dentro del template html
- **Angular Snippets (Version 16) (John Papa)**: los _snippets_ permiten generar código para servicios y componentes en forma rápida
- **ESLint (Microsoft)**: para disparar el linter de la sintaxis de TS
- **Git Lens (GitKraken)**, para ver el historial de Git integrado con tu Visual Studio Code
- **Material Icon Theme (Philipp Kief)**
- **Pretty TypeScript Errors**: mejora tu experiencia con los errores de Typescript
- **Prettier ESLint (Rebecca Vest)**: es el plugin que vamos a utilizar para aplicar el formato y ejecutar el proceso linter con la sintaxis de la cursada, que define
  - no usar puntos y coma al final de cada sentencia
  - utilizar comillas simples para strings, un estándar en el mundo Javascript/Typescript
  - evitar imports innecesarios o definiciones de variables que luego no se usen
  - etc. Más abajo te indicamos los dos archivos que definen esta configuración: `.eslintrc.json` y `.prettierrc.json`.

### Opcionales ###

- **Import Cost (Wix)**: permite calcular cuántos KB pesa cada import
- **Angular2-Switcher (infinity1207)**: agrega shortcuts para navegar entre .ts, .css, .html

## Alternativa a Visual Studio Code

Otra opción es utilizar [Web Storm](https://www.jetbrains.com/webstorm/) (de la suite de IntelliJ), si tienen una cuenta de la facultad pueden solicitar una licencia educativa. Solo que como no vamos a aprovechar todas las herramientas de este IDE poderoso quizás convenga ir por el Visual Studio Code.

# Aprendiendo Typescript

Typescript es el lenguaje de programación base para Angular. Tranquilo, es muy similar a los lenguajes orientados a objetos en los que ya trabajaste. Para iniciarte o para hacer consultas te dejamos estos links:

- [Documentación oficial de Typescript](https://www.typescriptlang.org/docs/): tiene una intro de 5 minutos, otros tutoriales cortos y el Handbook para sacarse dudas
- [Aprendiendo Typescript en 30 minutos](https://tutorialzine.com/2016/07/learn-typescript-in-30-minutes): muy buen tutorial para comenzar explicando los conceptos más salientes
- El [cheatsheet](https://rmolinamir.github.io/typescript-cheatsheet/) o guía rápida para tener a mano mientras programan
- **Tips**: [Typing destructured objects parameters](https://mariusschulz.com/blog/typing-destructured-object-parameters-in-typescript)

# Crear un proyecto Angular desde cero

En la consola Git Bash o bien desde una terminal de Linux hacemos

```bash
ng new nombre-de-tu-app
cd nombre-de-tu-app
npm start
```

## Correr los tests de un proyecto

Para ejecutar los tests de un proyecto, te posicionás en el directorio raíz y ejecutás desde la consola

```bash
npm test
```

## Archivo de configuración para Visual Studio Code

Te recomendamos que dentro del proyecto crees una carpeta `.vscode` y dentro un archivo `settings.json` que tenga este contenido:

```js
{
    "[javascript]": {
      "editor.defaultFormatter": "rvest.vs-code-prettier-eslint",
      "editor.formatOnPaste": false, // required 
      "editor.formatOnType": false, // required
      "editor.formatOnSave": true, // optional 
      "editor.formatOnSaveMode": "file", // required to format on save
    },
    "[typescript]": {
      "editor.defaultFormatter": "rvest.vs-code-prettier-eslint",
      "editor.formatOnPaste": false, // required 
      "editor.formatOnType": false, // required
      "editor.formatOnSave": true, // optional 
      "editor.formatOnSaveMode": "file", // required to format on save
    },
    "[json]": {
      "editor.defaultFormatter": "rvest.vs-code-prettier-eslint",
      "editor.formatOnPaste": false, // required 
      "editor.formatOnType": false, // required
      "editor.formatOnSave": true, // optional 
      "editor.formatOnSaveMode": "file", // required to format on save
    },
    "editor.codeActionsOnSave": {
      "source.fixAll": "explicit"
    }
}
```

## Cambios al package.json

Dentro del archivo `package.json` del raíz de tu proyecto debés tener estos scripts (**los últimos 4 son los que agregamos**):

```js
  "scripts": {
    "ng": "ng",
    "start": "ng serve",
    "build": "ng build",
    "watch": "ng build --watch --configuration development",
    "test": "ng test",
    "lint": "eslint \"**/*.{ts,tsx}\" ",
    "lint:fix": "eslint --fix \"**/*.{ts,tsx}\" ",
    "build:prod": "ng build --prod",
    "test:prod": "ng test --browsers=ChromeHeadless --watch=false --code-coverage"
  },
```

## Agregando Dependencias

Instalaremos algunas dependencias adicionales:

```bash
# Installar ESLint
npm install --save-dev eslint @typescript-eslint/parser

# Instalar plugins adicionales
npm install --save-dev @typescript-eslint/eslint-plugin eslint-plugin-prettier

# Instalar Prettier y sus dependencias
npm install --save-dev prettier prettier-eslint eslint-config-prettier
```

## Archivo .nvmrc

Tener un archivo `.nvmrc` es conveniente si todo el equipo trabaja con NVM (el versionador de Node). El contenido especifica qué versión de Node vamos a utilizar:

```bash
20.4.0
```

## Ejemplo de .gitignore

Te recomendamos que configures tu archivo .gitignore de la siguiente manera:

```bash
# See http://help.github.com/ignore-files/ for more about ignoring files.

# compiled output
/dist
/tmp
/out-tsc
# Only exists if Bazel was run
/bazel-out

# dependencies
/node_modules

# profiling files
chrome-profiler-events*.json

# IDEs and editors
/.idea
.project
.classpath
.c9/
*.launch
.settings/
*.sublime-workspace

# IDE - VSCode
.vscode/*
!.vscode/settings.json
!.vscode/tasks.json
!.vscode/launch.json
!.vscode/extensions.json
.history/*

# misc
/.sass-cache
/connect.lock
/coverage
/libpeerconnection.log
npm-debug.log
yarn-error.log
testem.log
/typings

# System Files
.DS_Store
Thumbs.db

# Angular cache
.angular
```

## Configuración del linter

El linter es el proceso que genera advertencias o errores en base a la sintaxis y semántica de nuestros componentes. Lo interesante es que podemos configurar, por ejemplo, que escribir `console.log` o `debugger` no es código que queremos que esté en el ambiente productivo, pero sí podríamos admitirlo en desarrollo. También se puede configurar validaciones como el uso de let en lugar de const, variables sin utilizar, etc. Te dejamos la configuración recomendada en el archivo `.eslintrc.json` que debe estar en el directorio raíz:

```js
{
  "parser": "@typescript-eslint/parser",
  "extends": [
    "plugin:@typescript-eslint/recommended"
  ],
  "parserOptions": {
    "ecmaVersion": 2021,
    "sourceType": "module"
  },
  "rules": {
    "semi": [
      2,
      "never"
    ],
    "@typescript-eslint/explicit-function-return-type": "off",
    "@typescript-eslint/explicit-module-boundary-types": "off",
    "@typescript-eslint/no-var-requires": "off"
  }
}
```

Para ejecutar el linter desde la línea de comandos, podés escribir

```bash
npm run lint
```

con el archivo `package.json` que contenga los scripts que arriba te dejamos. Es importante hacerlo **ya que el CI de Github Actions lo va a ejecutar para pasar el build**, para arreglar automáticamente todos los problemas que detecta el linter podés hacer

```bash
npm run lint:fix
```

## Configuración del archivo de test

Al archivo `karma.conf.js` que está en el directorio raíz hay que agregarle la opción para que genere el porcentaje de cobertura en formato `json` también:

```js
    coverageReporter: {
      dir: require('path').join(__dirname, './coverage/XXXXXX'), // <-- reemplazar XXXXXX por nombre del proyecto
      subdir: '.',
      reporters: [
        { type: 'html' },
        { type: 'text-summary' }, // <-- agregar una coma al final
        { type: 'lcov' }          // <-- agregar esta línea
      ]
    },
```

> Si no tenés un archivo `karma.conf.js` lo podés generar desde el Angular CLI: `ng generate config karma`.

# Otros archivos útiles

En la carpeta raíz creá los siguientes archivos 

- `.htmlhintrc` (configuración del Linter para HTML), con el siguiente contenido

```js
{
  "tagname-lowercase": false,
  "attr-lowercase": false
}
```

- `.prettierrc.json` (configuración de Prettier para eliminar puntos y coma, definir tab de 2 espacios, utilizar single quote, etc.) Es importante que tod@s tengan esta configuración para que no haya un montón de conflictos en git a la hora de pushear.

```js
{
  "singleQuote": true,
  "trailingComma": "none",
  "endOfLine": "auto",
  "tabWidth": 2,
  "semi": false
}
```

## Ejemplo de un archivo para Github Actions

Te dejamos [este archivo de ejemplo](./build_angular.yml) que tenés que guardar en `.github/workflows/build.yml`. Descargalo y reemplazá `XXXXXXXXX` por el nombre de la carpeta donde está tu proyecto.


## Cómo configurar los badges en tu README

- Para agregar el badge del build de Github Actions, seguí [estas instrucciones](https://docs.github.com/es/actions/managing-workflow-runs/adding-a-workflow-status-badge)

- Para agregar el badge del porcentaje de cobertura, tenés que agregar la imagen que genera el mismo build de Github Actions (reemplazando `XXXXXXX` por el nombre de la carpeta donde está tu proyecto):

```md
![Coverage](./badges/XXXXXXX/coverage.svg)
```

