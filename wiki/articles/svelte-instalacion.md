---
layout: article
title: Instalacion de Entorno Svelte
featured: true
categories: [web, svelte, ui, configuracion, entorno, client-side]
---

<img src="/img/languages/svelte.png" alt="logo svelte" height="auto" width="120px">

# Entorno

Es necesario que instales las siguientes herramientas, en este orden:

- Si estás en entorno Windows te recomendamos instalarte [Git Bash](https://gitforwindows.org/)
- Seguimos con [NodeJS](https://nodejs.org/en/).
  - Si estás en entorno Linux/Mac recomendamos que descargues Node desde [nvm (Node Version Manager)](https://github.com/nvm-sh/nvm) y luego instales esta versión: `nvm install 22.9.0`
  - Si estás en Windows instalate la versión actual
- Luego [NPM (Node Package Manager)](https://www.npmjs.com/), con el que vamos a hacer los builds de nuestras aplicaciones.
  - Para familiarizarte con el manejo de dependencias, te dejamos [este artículo](npm-dependencias.html)
	- Y como en realidad vamos a usar Yarn como manejador de dependencias, hay que instalarlo con npm

```bash
npm install --global yarn
```

# Editor de Texto

## Visual Studio Code

- El editor de texto que vamos a soportar en la cursada es [**Visual Studio Code**](https://code.visualstudio.com/) (hay [una versión portable](https://sourceforge.net/projects/vscode-portable/) si estás en una máquina sin privilegios de administrador).

Dentro de Visual Studio Code, te recomendamos que crees un perfil vacío y lo asocies a tus nuevos proyectos Svelte (podés ver [cómo se trabaja con perfiles en VSCode en este video](https://www.youtube.com/watch?v=_2F2Zt-_tUA). También te dejamos [este tutorial muy piola - en inglés](https://www.youtube.com/watch?v=QjvvqR9KyVo) y [la documentación oficial](https://code.visualstudio.com/docs/editor/profiles)).

### Instalación 

> **Opción 1**: podés importar [este archivo que trae todas las extensiones para Svelte](https://github.com/algo3-unsam/proyecto-base-tp/blob/master/Svelte.code-profile)

O si no, podés instalar las extensiones del Visual Studio Code manualmente. Para 2025 son los que ya instalaste para trabajar con HTML/CSS y los siguientes:

### Necesarios ###

- **Auto Rename Tag (Jun Han)**: que permite acomodar la apertura y cierre de los tags de HTML
- **ESLint (Microsoft)**: para disparar el linter de la sintaxis de TS
- **Github Actions (Github)**: te ayuda con el archivo de configuración de CI para Github Actions
- **GitLens - Git Supercharged (GitKraken)**: utilidades para el trabajo con Git
- **Playwright Runner by Koushik (Koushik Chatterjee)**: ayuda a ejecutar tests e2e de Playwright
- **Playwright Test for VSCode (Microsoft)**: plugin para ejecutar tests e2e de Playwright
- **Svelte for VS Code (Svelte)**: plugin para soportar Svelte en VSCode
- **Vitest (Vitest)**: plugin para poder ejecutar los tests de frontend en el VSCode

### Opcionales ###

- **Playwright Snippets (Nitay Neeman)**: snippets de código que expanden tests e2e de Playwright
- **Svelte snippets**: autocompletado de código para Svelte
- **Svelte Dark**: tema oscuro de Svelte

# Aprendiendo Typescript

Typescript es el lenguaje de programación base en el que vamos a trabajar. Tranquilo, es muy similar a los lenguajes orientados a objetos en los que ya trabajaste. Para iniciarte o para hacer consultas te dejamos estos links:

- [Documentación oficial de Typescript](https://www.typescriptlang.org/docs/): tiene una intro de 5 minutos, otros tutoriales cortos y el Handbook para sacarse dudas
- [Aprendiendo Typescript en 30 minutos](https://tutorialzine.com/2016/07/learn-typescript-in-30-minutes): muy buen tutorial para comenzar explicando los conceptos más salientes
- El [cheatsheet](https://rmolinamir.github.io/typescript-cheatsheet/) o guía rápida para tener a mano mientras programan
- **Tips**: [Typing destructured objects parameters](https://mariusschulz.com/blog/typing-destructured-object-parameters-in-typescript)

# Crear un proyecto Svelte desde cero

```bash
npx sv create lala
┌  Welcome to the Svelte CLI! (v0.6.5)
│
◇  Which template would you like?
│  SvelteKit minimal
│
◇  Add type checking with Typescript?
│  Yes, using Typescript syntax
│
◆  Project created
│
◆  What would you like to add to your project? (use arrow keys / space bar)
│  ◻ prettier
│  ◼ eslint
│  ◼ vitest
│  ◼ playwright (browser testing - https://playwright.dev)
│  ◻ tailwindcss
│  ◻ drizzle
│  ◻ lucia
│  ◻ mdsvex
│  ◻ paraglide
│  ◻ storybook
└
◇  Which package manager do you want to install dependencies with?
│  yarn
│
◆  Successfully setup add-ons
│
◇  Installing dependencies with yarn...
```

Repasamos las opciones
- Sveltekit minimal como biblioteca
- usar Typescript (sin JSDoc)
- agregamos eslint, vitest y playwright. Vitest es para hacer tests de frontend y Playwright para tests e2e
- yarn como manejador de dependencias (es un poco más rápido que npm)

## Archivo de configuración para Visual Studio Code

Te recomendamos que dentro del proyecto crees una carpeta `.vscode` y dentro un archivo `settings.json` que tenga este contenido:

```js
{
	"editor.codeActionsOnSave": {
		"source.fixAll.eslint": "explicit",
		"source.removeUnusedImports": "explicit",
	},
	"editor.defaultFormatter": "dbaeumer.vscode-eslint",
	"editor.formatOnSave": true,
	"editor.tabSize": 2,
	"eslint.validate": [
		"javascript",
		"javascriptreact",
		"typescript"
	],
	"[svelte]": {
		"editor.defaultFormatter": "svelte.svelte-vscode"
	}
}
```

## Configuración para Typescript

El archivo `tsconfig.json` define cómo vamos a utilizar el intérprete de Typescript. Es conveniente agregar al final estas líneas:

```ts
	"compilerOptions": {
		...,
		"module": "ES2015",
		"lib": [
			"ES2015"
		],
		"moduleResolution": ...
```

## Cambios al package.json

Dentro del archivo `package.json` del raíz de tu proyecto hay que agregar `lint:fix` (para poder corregir errores simples del linter) y `test:ci` (para que al ejecutar el build de Github Actions tengas la cobertura):

```js
  "scripts": {
		"lint": "eslint src",
		"lint:fix": "eslint src --fix",
		"test:unit": ...,
		"test:ci": "npm run test:unit -- --run --coverage"
  },
```

Cuando agregues los tests e2e podés definir estos scripts:

```js
  "scripts": {
		"test:e2e": "playwright test --ui",
		"test": "npm run test:e2e",
		"test:ci": "npm run test:unit -- --run --coverage && playwright test"
	}
```

Con eso vas a correr tanto los test escritos con Vitest como los de Playwright (los veremos más adelante).

## Cambios al archivo de configuración de Svelte

Es conveniente hacer el siguiente cambio al archivo `svelte.config.js`:

```ts
...
const config = {
	// agregamos `{ script: true } al preprocesador para tener acceso a los syntactic sugar de TS
	preprocess: vitePreprocess({ script: true }),
	//
```

## Dependencias adicionales

Ejecutá este comando para agregar las siguientes dependencias:

```bash
yarn add @testing-library/jest-dom @testing-library/svelte @testing-library/user-event @types/eslint @vitest/coverage-v8 jsdom
```

Para agregar dependencias de los tests e2e:

```bash
yarn add @playwright/test
```

## Archivo .nvmrc

Tener un archivo `.nvmrc` es conveniente si todo el equipo trabaja con NVM (el versionador de Node). El contenido especifica qué versión de Node vamos a utilizar:

```bash
22.9.0
```

## Ejemplo de .gitignore

Agregamos estas líneas al archivo `.gitignore`:

```bash
vite.config.ts.timestamp-*
# ... empezamos acá ...

# Coverage
coverage

# VSCode
.history

# Playwright
test-results
```

## Configuración del linter

Cada vez que grabamos un archivo se ejecuta automáticamente el proceso que analiza el código y diagnostica errores de sintaxis y oportunidades de mejora, proceso que se conoce como **Linter**. El archivo `eslint.config.js` debe tener el siguiente contenido:

```js
// eslint.config.cjs

// import eslintPluginPrettierRecommended from 'eslint-plugin-prettier/recommended'
import eslintPluginSvelte from 'eslint-plugin-svelte'
import js from '@eslint/js'
import svelteParser from 'svelte-eslint-parser'
import tsEslint from 'typescript-eslint'
import tsParser from '@typescript-eslint/parser'
import globals from 'globals'

export default [
  js.configs.recommended,
  ...tsEslint.configs.strict,
  ...eslintPluginSvelte.configs['flat/recommended'],
  {
    languageOptions: {
      globals: {
        ...globals.browser,
      },
    },
    rules: {
      quotes: [
        'warn',
        'single',
        { avoidEscape: true, allowTemplateLiterals: true },
      ],
      semi: ['error', 'never'],
      indent: ['warn', 2],
      'no-extra-parens': 'warn',
      'no-nested-ternary': 'error',
      'linebreak-style': ['error', 'unix'],
      'no-cond-assign': ['error', 'always'],
      'no-console': 'error',
      '@typescript-eslint/sort-type-constituents': 'error',
    },
  },
  {
    files: ['**/*.svelte'],
    languageOptions: {
      parser: svelteParser,
      parserOptions: {
        parser: tsParser,
      },
    },
    rules: {
      'svelte/no-target-blank': 'error',
      'svelte/no-at-debug-tags': 'error',
      'svelte/no-reactive-functions': 'error',
      'svelte/no-reactive-literals': 'error',
      '@/semi': ['error', 'never'],
      '@/quotes': ['warn', 'single'],
      '@/indent': ['warn', 2],
    }
  }
]
```

## Configuración de prettier

Indirectamente el plugin de Svelte para VSCode utiliza [**Prettier**](https://prettier.io/), un formateador de código. Agregamos entonces un archivo `.prettierrc` con el siguiente contenido:

```json
{
	"useTabs": false,
	"singleQuote": true,
	"tabWidth": 2,
	"semi": false,
	"trailingComma": "none",
	"printWidth": 100,
	"plugins": [
		"prettier-plugin-svelte"
	],
	"overrides": [
		{
			"files": "*.svelte",
			"options": {
				"parser": "svelte"
			}
		}
	]
}
```

## Ejecutar el linter

Si queremos ejecutar el proceso que corrige los errores del linter podemos hacerlo desde la línea de comando:

```bash
yarn run lint:fix
```

## Vitest: configuración del archivo

El archivo `vite.config.ts` tiene que incorporar el plugin de testing de Svelte. Te dejamos el archivo completo:

```ts
import { defineConfig } from 'vitest/config'
import { svelteTesting } from '@testing-library/svelte/vite'
import { sveltekit } from '@sveltejs/kit/vite'

export default defineConfig({
	plugins: [sveltekit(), svelteTesting()],

	test: {
		include: ['src/**/*.{test,spec}.{js,ts}'],
		globals: true,
		environment: 'jsdom',
		setupFiles: ['./vitest-setup.js'],
		coverage: {
			include: ['src']
		}
	}
})
```

## Ejemplo de un archivo para Github Actions

Te dejamos [este archivo de ejemplo](./build_svelte.yml) que tenés que guardar en `.github/workflows/build.yml`. Descargalo y reemplazá `XXXXXXXXX` por el nombre de la carpeta donde está tu proyecto. Después veremos cómo agregar para que ejecuten los tests e2e.

## Cómo configurar los badges en tu README

- Para agregar el badge del build de Github Actions, seguí [estas instrucciones](https://docs.github.com/es/actions/managing-workflow-runs/adding-a-workflow-status-badge)

- Para agregar el badge del porcentaje de cobertura, tenés que agregar la imagen que genera el mismo build de Github Actions (reemplazando `XXXXXXX` por el nombre de la carpeta donde está tu proyecto):

```md
![Coverage](./badges/XXXXXXX/coverage.svg)
```

## Levantar la app

Para levantar la aplicación Svelte ejecutás

```bash
yarn run dev
```

Luego en un navegador pedís la siguiente url: `http://localhost:5173`.

## Correr los tests de un proyecto

Para ejecutar los tests de un proyecto, te posicionás en el directorio raíz y ejecutás desde la consola

```bash
yarn test
```

