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
- Seguimos con [NodeJS](https://nodejs.org/en/), preferentemente la última versión (tiene que ser 20.4.0 ó superior).
- Luego [NPM (Node Package Manager)](https://www.npmjs.com/), con el que vamos a hacer los builds de nuestras aplicaciones.
- El editor de texto que vamos a soportar en la cursada es [Visual Studio Code](https://code.visualstudio.com/) (hay una versión portable si estás en una máquina sin privilegios de administrador).

# Específicos de React

# Plugins Visual Studio Code

Dentro de Visual Studio Code, las extensiones que recomendamos para trabajar con React deberían ser las mismas con las que trabajaste en Angular y además revisar que tengas:

- **ESLint - Microsoft**
- **Prettier - ESLint - Rebecca Vest**
- y además **Vitest - Zixuan Chen**

# Crear un proyecto React de cero

Para crear un proyecto React desde la consola Git Bash o bien desde una terminal Linux escribimos:

```bash
npm create vite@latest nombre-del-proyecto
```

Y luego

- `✔ Select a framework: › React`: seleccionar React como framework de UI
- `✔ Select a variant: › Javascript + SWC`: elegir la variante Javascript con SWC (herramienta de reemplazo de Babel)

También podés usar el template directo React + Javascript + SWC (eso evita que tengas que seleccionar el tipo de proyecto):

```bash
npm create vite@latest nombre-de-proyecto -- --template react-swc
```

Por defecto la aplicación cliente levantará en el puerto 5173. Como suele quedarse levantada aun cuando canceles la línea de comando y el navegador, te dejamos este link que te dice [cómo bajar el proceso del sistema operativo](https://stackoverflow.com/questions/39322089/node-js-port-3000-already-in-use-but-it-actually-isnt) para correr otro ejemplo.

# Configuraciones adicionales para Algoritmos III

Una vez creado el proyecto, te recomendamos que agregues estas configuraciones.

## Agregar dependencias

Agregamos estas dependencias de Prettier y Vitest (el framework de testing)

```bash
npm i @testing-library/react @testing-library/jest-dom @testing-library/user-event @vitest/coverage-v8 @vitejs/plugin-react jsdom prettier vitest -D
```

## Archivo .nvmrc

Tener un archivo `.nvmrc` es conveniente si todo el equipo trabaja con NVM (el versionador de Node). El contenido especifica qué versión de Node vamos a utilizar:

```bash
20.4.0
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

El archivo `.eslintrc.cjs` debe tener la siguiente configuración:

```js
module.exports = {
  root: true,
  env: { browser: true, es2020: true },
  extends: [
    'eslint:recommended',
    'plugin:react/recommended',
    'plugin:react/jsx-runtime',
    'plugin:react-hooks/recommended',
  ],
  ignorePatterns: ['dist', '.eslintrc.cjs'],
  parserOptions: { ecmaVersion: 'latest', sourceType: 'module' },
  settings: { react: { version: '18.2' } },
  plugins: ['react-refresh'],
  rules: {
    'semi': ['error', 'never'],
    'prefer-const': [
      'warn',
      {
        'destructuring': 'any',
        'ignoreReadBeforeAssign': false
      }
    ],
    'getter-return': ['error'],
    'no-async-promise-executor': ['warn'],
    'no-cond-assign': ['warn', 'except-parens'],
    'no-dupe-keys': ['error'],
    'no-empty': ['warn'],
    'no-ex-assign': ['error'],
    'no-extra-boolean-cast': [
      'error',
      {
        'enforceForLogicalOperands': true
      }
    ],
    'no-extra-parens': ['warn', 'all'],
    'no-func-assign': ['error'],
    'no-import-assign': ['error'],
    'no-inner-declarations': ['error', 'both'],
    'no-obj-calls': ['error'],
    'no-promise-executor-return': ['error'],
    'no-sparse-arrays': ['error'],
    'no-template-curly-in-string': ['error'],
    'no-unreachable': ['error'],
    'no-unreachable-loop': ['error'],
    'no-unsafe-optional-chaining': ['error'],
    'require-atomic-updates': ['error'],
    'use-isnan': ['error'],
    'block-scoped-var': ['error'],
    'consistent-return': ['error'],
    'default-param-last': ['warn'],
    'no-alert': ['error'],
    'no-caller': ['error'],
    'no-constructor-return': ['error'],
    'no-global-assign': ['error'],
    'no-lone-blocks': ['warn'],
    'no-multi-spaces': ['warn'],
    'no-new': ['warn'],
    'no-param-reassign': ['error'],
    'no-proto': ['warn'],
    'no-redeclare': ['error'],
    'no-return-assign': ['error'],
    'no-return-await': ['warn'],
    'no-self-assign': ['error'],
    'no-sequences': ['warn'],
    'no-useless-catch': ['warn'],
    'no-useless-return': ['error'],
    'no-void': ['error'],
    'no-with': ['error'],
    'no-undef': ['off'],
    'react/display-name': ['off'],
    'react/jsx-uses-react': 'off',
    'react/react-in-jsx-scope': 'off',
    'react-refresh/only-export-components': [
      'warn',
      { allowConstantExport: true },
    ],
  },
}
```

## Configuración del proyecto

Al archivo `package.json` le modificamos el script lint y le agregamos test y coverage (borrale los comentarios porque no están permitidos en `json`):

```js
  "scripts": {
    "lint": "eslint --cache --fix .", // modificar
    ...
    // agregar
    "test": "vitest",
    "coverage": "vitest run --coverage"
  },
```

## .gitignore

Al archivo .gitignore se le pueden incorporar estas líneas:

```bash
# VSC - Git Lens
.history
```

## Archivo de configuración para Visual Studio Code

Te recomendamos que dentro del proyecto crees una carpeta `.vscode` y dentro un archivo `settings.json` que tenga este contenido:

```js
{
    "editor.codeActionsOnSave": { "source.fixAll.eslint": true },
    "editor.formatOnSave": true, 
    "files.autoSave": "onFocusChange" 
}
```

## Configuración para el testeo unitario de frontend

Hay que definir un archivo `setupTests.js` en el raíz de nuestro proyecto, que tenga el siguiente contenido:

```js
import "@testing-library/jest-dom/vitest"
```

El archivo `vite.config.js` contiene la configuración que necesitamos para ejecutar los tests, es importante agregar un reporter de tipo `json-summary` para que el build de Github Actions cree el badge con el % de cobertura automáticamente:

```js
/// <reference types="vitest" />
/// <reference types="vite/client" />

import { defineConfig } from 'vite'

import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  test: {
    globals: true,
    setupFiles: ["./setupTests.js"], // es importante definirlo en un archivo aparte para que se ejecute en otro contexto
    environment: 'jsdom',
    coverage: {
      reporter: ['text', 'json', 'html', 'json-summary'],
    },
  }
})
```

> Si no configurás el archivo `setupTests.js` te va a aparecer un mensaje de error: `Error: Invalid Chai property: toBeInTheDocument` en cada expect.

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
