---
layout: article
title: Instalacion de Entorno Angular
featured: true
categories: [web, angular, ui, configuracion, entorno, spa]
---

![angular](/img/languages/angular1.png)

# Entorno

Es necesario que instales las siguientes herramientas, en este orden:

- Si estás en entorno Windows te recomendamos instalarte [Git Bash](https://gitforwindows.org/)
- Seguimos con [NodeJS](https://nodejs.org/en/).
  - Si estás en entorno Linux/Mac recomendamos que descargues Node desde [nvm (Node Version Manager)](https://github.com/nvm-sh/nvm) y luego instales esta versión: `nvm install lts/fermium -> v14.15.2`
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

Los plugins del Visual Studio Code que te recomendamos al 2021 son:

### Necesarios ###

- **Path Intellisense (Christian Kohler)**: autocompletado para archivos de tu file system
- **Auto Import (steoates)**: ayuda y autocompletado para importar componentes de JS
- **Angular Files (Alexander Ivanichev)**: agrega un menú contextual para crear elementos de Angular
- **JSON to TS (MariusAlchimavicius)**: te construye una interfaz de TS en base a la información de un JSON
- **Angular Language Service (Angular)**: autocompletado dentro del template html
- **Material Icon Theme (Philipp Kief)**
- **Git Lens**, para ver el historial de Git integrado con tu Visual Studio Code

### Opcionales ###

- **Import Cost (Wix)**: permite calcular cuántos KB pesa cada import
- **Angular2-Switcher**: agrega shortcuts para navegar entre .ts, .css, .html
- **Angular2 Inline**: syntax highlighting y autocompletado de código para componentes Angular inline (que tienen embebido html y css)
- **REST Client**: para hacer pedidos http desde Visual Studio Code directamente (podés usar POSTMAN, Insomnia o Swagger + navegador también)

## Alternativa a Visual Studio Code

Otra opción es utilizar [Web Storm](https://www.jetbrains.com/webstorm/) (de la suite de IntelliJ), si tienen una cuenta de la facultad pueden solicitar una licencia educativa. Solo que como no vamos a aprovechar todas las herramientas de este IDE poderoso quizás convenga ir por el Visual Studio Code.

# Aprendiendo Typescript

Typescript es el lenguaje de programación base para Angular. Tranquilo, es muy similar a los lenguajes orientados a objetos en los que ya trabajaste. Para iniciarte o para hacer consultas te dejamos estos links:

- [Documentación oficial de Typescript](https://www.typescriptlang.org/docs/home.html): tiene una intro de 5 minutos, otros tutoriales cortos y el Handbook para sacarse dudas
- [Aprendiendo Typescript en 30 minutos](https://tutorialzine.com/2016/07/learn-typescript-in-30-minutes): muy buen tutorial para comenzar explicando los conceptos más salientes
- [Tutorial de Typescript en castellano y PDF](https://www.gitbook.com/download/pdf/book/khru/typescript): material de consulta en castellano para los interesados
- El [cheatsheet](https://rmolinamir.github.io/typescript-cheatsheet/) o guía rápida para tener a mano mientras programan
- Y como typescript es un superconjunto de javascript, siempre conviene tener a mano las [funcionalidades de ES6](http://es6-features.org)
- **Tips**
  - [Typing destructured objects parameters](https://mariusschulz.com/blog/typing-destructured-object-parameters-in-typescript)

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
ng test
```

# Archivo de configuración para Visual Studio Code

Te recomendamos que dentro del proyecto crees una carpeta `.vscode` y dentro un archivo `settings.json` que tenga este contenido:

```js
{
  "[javascript]": {
    "editor.defaultFormatter": "dbaeumer.vscode-eslint",
    "editor.codeActionsOnSave": {
      "source.fixAll.eslint": true
    },
    "editor.formatOnSave": false
  },
  "[typescript]": {
    "editor.defaultFormatter": "dbaeumer.vscode-eslint",
    "editor.codeActionsOnSave": {
      "source.fixAll.eslint": true
    },
    "editor.formatOnSave": false
  },
  "[json]": {
    "editor.defaultFormatter": "dbaeumer.vscode-eslint",
    "editor.codeActionsOnSave": {
      "source.fixAll.eslint": true
    },
    "editor.formatOnSave": false
  }
}
```

# Cambios al package.json

Dentro del archivo `package.json` del raíz de tu proyecto debés tener estos scripts:

```js
  "scripts": {
    "ng": "ng",
    "start": "ng serve",
    "build": "ng build",
    "watch": "ng build --watch --configuration development",
    "test": "ng test",
    "lint": "eslint \"**/*.{ts,tsx}\" ",
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

# Ejemplo de .gitignore

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
```

# Configuración del linter

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
    "@typescript-eslint/explicit-module-boundary-types": ["off"],
    "@typescript-eslint/no-var-requires": "off"
  }
}
```

# Configuración del archivo de test

Al archivo `karma.conf.js` que está en el directorio raíz hay que agregarle la opción para que genere el porcentaje de cobertura en formato `json` también:

```js
    coverageReporter: {
      dir: require('path').join(__dirname, './coverage/eg-conversor-angular'),
      subdir: '.',
      reporters: [
        { type: 'html' },
        { type: 'text-summary' }, // <-- agregar una coma al final
        { type: 'json-summary' }  // <-- agregar esta línea
      ]
    },
```

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

# Ejemplo de un archivo para Github Actions

Para agregar el coverage tenés que reemplazar `XXXXXXXXX` por el nombre de la carpeta donde está tu proyecto.

Este es un archivo de ejemplo que tenés que guardar en `.github/workflows/build.yml`:

```yml
name: Build
on:
  push:
  pull_request:
    branches:
      - master

jobs:
  build:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        node-version: [14.x]

    steps:
      - uses: actions/checkout@v1

      - name: Cache node modules
        uses: actions/cache@v1
        with:
          path: ~/.npm
          key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}
          restore-keys: |
            ${{ runner.os }}-node-

      - name: Node ${{ matrix.node-version }}
        uses: actions/setup-node@v1
        with:
          node-version: ${{ matrix.node-version }}

      - name: Run tests & linter
        run: |
          npm ci
          npm run test:prod
          npm run lint
      - name: Coverage badge
        uses: demyanets/angular-coverage-badges-action@v1
        with:
          coverage-summary-path: coverage/XXXXXXXXX/coverage-summary.json
          protected-branches: '["master"]'
          github_token: ${{ secrets.GITHUB_TOKEN }}
```


# Cómo configurar los badges en tu README

- Para agregar el badge del build de Github Actions, seguí [estas instrucciones](https://docs.github.com/es/actions/managing-workflow-runs/adding-a-workflow-status-badge)

- Para agregar el badge del porcentaje de cobertura, tenés que agregar la imagen que genera el mismo build de Github Actions (reemplazando `XXXXXXX` por el nombre de la carpeta donde está tu proyecto):

```md
![Coverage](./badges/XXXXXXX/coverage.svg)
```


# Ejemplo de un archivo Travis (deprecado)

El siguiente es un ejemplo posible de un archivo `.travis.yml` para una aplicación Angular 8 ó superior. **Si estás cursando Algoritmos III podés ignorar este paso**:

```yml
sudo: required
dist: trusty
language: node_js
node_js:
  - '10.12'  # posiblemente haya que subirle la versión de Node

addons:
apt:
  sources:
    - google-chrome
  packages:
    - google-chrome-stable
    - google-chrome-beta

before_install:
  - npm install -g npm@latest
  - export CHROME_BIN=chromium-browser
  - export DISPLAY=:99.0
  - sh -e /etc/init.d/xvfb start

before_script:

script: ng test --sourceMap=false --watch=false
```

Lo importante es que la versión de node que uses sea superior a 10.1
