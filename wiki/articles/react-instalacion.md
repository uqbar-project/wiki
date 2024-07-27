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
- Seguimos con [NodeJS](https://nodejs.org/en/), preferentemente la última versión (tiene que ser 20.x).
- Luego [NPM (Node Package Manager)](https://www.npmjs.com/), con el que vamos a hacer los builds de nuestras aplicaciones.
- El editor de texto que vamos a soportar en la cursada es [Visual Studio Code](https://code.visualstudio.com/) (hay una versión portable si estás en una máquina sin privilegios de administrador).

# Específicos de React

# Plugins Visual Studio Code

Dentro de Visual Studio Code, te recomendamos que crees un perfil vacío y lo asocies a tus nuevos proyectos React (podés ver [cómo se trabaja con perfiles en VSCode en este video](https://www.youtube.com/watch?v=_2F2Zt-_tUA), la recomendación es partir con un profile totalmente vacío. También te dejamos [este tutorial muy piola (en inglés)](https://www.youtube.com/watch?v=QjvvqR9KyVo)). Las extensiones que recomendamos para trabajar son:

- **ESLint - Microsoft**: integra las reglas definidas en el linter con el IDE
- **Prettier - ESLint - Rebecca Vest**: formatea el código según la configuración de Prettier
- **npm intellisense - de Christian Kohler**: te permite autocompletar dependencias buscándolas en npm
- **VSCode React Refactor**: agrega refactors de los componentes de React
- **Vitest - Vitest**: para ejecutar los tests de frontend desde el VSC
- y por supuesto **Git Lens - Git supercharged** para manejarte con git

Opcionalmente podés instalarte

- **Import Cost - Wix**: te dice cuánto pesan (en KB) los imports de cada archivo que hacés en tus componentes


# Crear un proyecto React de cero

Para crear un proyecto React desde la consola Git Bash o bien desde una terminal Linux escribimos:

```bash
npm create vite@latest nombre-del-proyecto
```

Y luego

- `✔ Select a framework: › React`: seleccionar React como framework de UI
- `✔ Select a variant: › Typescript + SWC`: elegir la variante Typescript con SWC (herramienta de reemplazo de Babel)

También podés usar el template directo React + Typescript + SWC (eso evita que tengas que seleccionar el tipo de proyecto):

```bash
npm create vite@latest nombre-de-proyecto -- --template react-swc-ts
```

Por defecto la aplicación cliente levantará en el puerto 5173. Como suele quedarse levantada aun cuando canceles la línea de comando y el navegador, te dejamos este link que te dice [cómo bajar el proceso del sistema operativo](https://stackoverflow.com/questions/39322089/node-js-port-3000-already-in-use-but-it-actually-isnt) para correr otro ejemplo.

# Configuraciones adicionales para Algoritmos III

Una vez creado el proyecto, te recomendamos que agregues estas configuraciones.

## Agregar dependencias

Agregamos estas dependencias de Prettier y Vitest (el framework de testing)

```bash
npm i @testing-library/react @testing-library/jest-dom @testing-library/user-event @vitest/coverage-v8 @vitejs/plugin-react-swc jsdom prettier vitest -D
```

## Archivo .nvmrc

Tener un archivo `.nvmrc` es conveniente si todo el equipo trabaja con NVM (el versionador de Node). El contenido especifica qué versión de Node vamos a utilizar:

```bash
20.4.0
```

Para la cursada 2024 vamos a trabajar con Node 20, que [será la versión activa al menos hasta mediados de Octubre](https://nodejs.org/en/about/previous-releases). Para el 2025 migraremos a la versión 22.

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
    'plugin:@typescript-eslint/recommended',
    'plugin:react-hooks/recommended',
  ],
  ignorePatterns: ['dist', '.eslintrc.cjs'],
  parser: '@typescript-eslint/parser',
  plugins: ['react-refresh'],
  rules: {
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
    "lint": "eslint . --ext ts,tsx --report-unused-disable-directives --max-warnings 0 --fix", // agregarle --fix
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

El archivo `vite.config.js` contiene la configuración que necesitamos para ejecutar los tests, es importante tener un reporter que sea válido para la herramienta de cobertura que utilices (puede ser `json-summary` o `lcov`):

```js
/// <reference types="vitest" />
/// <reference types="vite/client" />

import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      src: '/src',
      components: '/src/components',
    },
  },
  test: {
    globals: true,
    environment: 'jsdom',
    coverage: {
      reporter: ['lcov', 'json', 'html', 'json-summary'],
    },
  }
})
```

Por otra parte, los imports conviene hacerlos en forma absoluta, desde `src` para las definiciones de dominio, services, etc. y `src/components` para los componentes de React.


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
