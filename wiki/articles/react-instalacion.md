---
layout: article
title: Instalacion de ReactJS
categories: [arena, entorno, instalacion]
featured: true
---

<img src="/img/languages/React-logo.png" height="30%" width="30%"/>

# Pasos previos

Si ya estuviste trabajando con Angular estos pasos no son necesarios, pero conviene verificar que ya estén instalados.

- Si estás en entorno Windows te recomendamos instalarte [Git Bash](https://gitforwindows.org/)
- Seguimos con [NodeJS](https://nodejs.org/en/), preferentemente la última versión.
- Luego [NPM (Node Package Manager)](https://www.npmjs.com/), con el que vamos a hacer los builds de nuestras aplicaciones.
- El editor de texto que vamos a soportar en la cursada es [Visual Studio Code](https://code.visualstudio.com/) (hay una versión portable si estás en una máquina sin privilegios de administrador).

# Específicos de React

## npx

npx permite ejecutar paquetes binarios de npm mediante un command-line interface, y se instala con npm

```bash
npm install -g npx
```

## yarn

Yarn es un manejador de paquetes similar a npm, se instala globalmente desde la consola:

```bash
npm install --global yarn
```

# Plugins Visual Studio Code

Dentro de Visual Studio Code, las extensiones que recomendamos para trabajar con React deberían ser:

- compartidas con el entorno de Angular
  - **ESLint - Dirk Baeumer**
  - **Prettier - Code formatter - Prettier**
  - **Prettier - ESLint - Rebecca Vest**
- compartidas con el entorno de Typescript
  - [JEST](https://marketplace.visualstudio.com/items?itemName=Orta.vscode-jest): que permite integrar en VSC los test unitarios que vamos a ejecutar con Jest.
  - [Jest Runner](https://marketplace.visualstudio.com/items?itemName=firsttris.vscode-jest-runner): para ejecutar o debuggear tests unitarios contra la terminal (cuando tengamos muchos tests y el primer plugin resulte engorroso).

Opcionalmente podés instalar la extensión _ES7 React/Redux/GraphQL/React-Native snippets_ de _dsznajder_.

# Crear un proyecto React de cero

Para crear un proyecto React desde la consola Git Bash o bien desde una terminal Linux escribimos:

```bash
npx create-react-app nombre-de-tu-app
cd nombre-de-tu-app
yarn start
```

Por defecto la aplicación cliente levantará en el puerto 3000. Como suele quedarse levantada aun cuando canceles la línea de comando y el navegador, te dejamos este link que te dice [cómo bajar el proceso del sistema operativo](https://stackoverflow.com/questions/39322089/node-js-port-3000-already-in-use-but-it-actually-isnt) para correr otro ejemplo.

# Configuraciones adicionales para Algoritmos III

Una vez creado el proyecto, te recomendamos que agregues estas configuraciones.

## Agregar dependencias

Agregamos una dependencia

```bash
yarn add eslint-plugin-react -D
```

## Archivos útiles

En la carpeta raíz creá los siguientes archivos 

- `.markdownlint.json` (configuración del Linter para archivos con extensión `.md`), con el siguiente contenido

```js
{
  "MD013": false,
  "MD024": false,
  "MD025": false
}
```

- `.prettierrc` (configuración de Prettier para eliminar puntos y coma, definir tab de 2 espacios, utilizar single quote, etc.) Es importante que tod@s tengan esta configuración para que no haya un montón de conflictos en git a la hora de pushear.

```js
{
  "trailingComma": "all",
  "tabWidth": 2,
  "semi": false,
  "singleQuote": true
}
```

## Linter para javascript

El archivo `.eslintrc.json` debe tener la siguiente configuración:

```js
{
    "parser": "babel-eslint",
    "extends": [
        "eslint:recommended",
        "plugin:react/recommended"
    ],
    "ignorePatterns": ["/build/**"],
    "plugins": [
        "react"
    ],
    "settings": {
        "react": {
        "version": "detect"
        }
    },
    "parserOptions": {
        "ecmaVersion": 6,
        "sourceType": "module",
        "ecmaFeatures": {
            "jsx": true
        }
    },
    "rules": {
        "semi": [ "error", "never" ],
        "prefer-const": ["warn", {
            "destructuring": "any",
            "ignoreReadBeforeAssign": false
        }],
        "getter-return": [ "error" ],
        "no-async-promise-executor": [ "warn" ],
        "no-cond-assign": [ "warn", "except-parens" ],
        "no-dupe-keys": [ "error" ],
        "no-empty": [ "warn" ],
        "no-ex-assign": [ "error" ],
        "no-extra-boolean-cast": [ "error", {
            "enforceForLogicalOperands": true 
        }],
        "no-extra-parens": [ "warn", "all" ],
        "no-func-assign": [ "error" ],
        "no-import-assign": [ "error" ],
        "no-inner-declarations": [ "error", "both" ],
        "no-obj-calls": [ "error" ],
        "no-promise-executor-return": [ "error" ],
        "no-sparse-arrays": [ "error" ],
        "no-template-curly-in-string": [ "error" ],
        "no-unreachable": [ "error" ],
        "no-unreachable-loop": [ "error" ],
        "no-unsafe-optional-chaining": [ "error" ],
        "require-atomic-updates": [ "error" ],
        "use-isnan": [ "error" ],
        "block-scoped-var": [ "error" ],
        "consistent-return": [ "error" ],
        "default-param-last": [ "warn" ],
        "no-alert": [ "error" ],
        "no-caller": [ "error"],
        "no-constructor-return": [ "error" ],
        "no-global-assign": [ "error" ],
        "no-lone-blocks": [ "warn" ],
        "no-multi-spaces": [ "warn" ],
        "no-new": [ "warn" ],
        "no-param-reassign": [ "error" ],
        "no-proto": [ "warn" ],
        "no-redeclare": [ "error" ],
        "no-return-assign": [ "error" ],
        "no-return-await": [ "warn" ],
        "no-self-assign": [ "error" ],
        "no-sequences": [ "warn" ],
        "no-useless-catch": [ "warn" ],
        "no-useless-return": [ "error" ],
        "no-void": [ "error" ],
        "no-with": [ "error" ],
        "no-undef": [ "off" ]
    }
}
```

## Configuración del proyecto

Al archivo `package.json` le vamos a configurar JEST (el framework de testeo unitario) para tener un coverage más exacto (borrale los comentarios porque no están permitidos en `json`):

```js
  "scripts": {
    ...,
    "eject": "react-scripts eject", // se agrega una coma
    "lint": "eslint .",             // línea a agregar para ejecutar el linter
    "lint:fix": "eslint . --fix"    // línea a agregar para ejecutar el fix del linter
  },
  "eslintConfig": {
    // dejar la configuración de eslintConfig tal cual está
  },
  "jest": {
    "coverageReporters": [ "html", "text-summary", "json-summary"],
    "collectCoverageFrom": [
      "**/*.{js,jsx}",
      "!**/reportWebVitals.js",
      "!**/index.js",
      "!**/setupTests.js",
      "!**/node_modules/**",
      "!**/vendor/**"
    ]
  },
```

Luego ejecutá por la línea de comando

```bash
yarn run lint:fix
```

para que el linter corrija los errores y advertencias automáticamente.

## .gitignore

Al archivo .gitignore se le pueden incorporar estas líneas:

```bash
# VSC - Git Lens
.history
```

# Ejemplo de un archivo para Github Actions

Te dejamos [este archivo de ejemplo](./build_react.yml) que tenés que guardar en `.github/workflows/build.yml`. No hay que hacer ningún cambio.

# Cómo configurar los badges en tu README

- Para agregar el badge del build de Github Actions, seguí [estas instrucciones](https://docs.github.com/es/actions/managing-workflow-runs/adding-a-workflow-status-badge)

- Para agregar el badge del porcentaje de cobertura, tenés que agregar la imagen que genera el mismo build de Github Actions (tal cual está escrito):

```md
![coverage](./badges/coverage/coverage.svg)
```

## Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
