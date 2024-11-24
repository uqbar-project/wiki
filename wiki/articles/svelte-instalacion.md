---
layout: article
title: Instalacion de Entorno Svelte
featured: true
categories: [web, svelte, ui, configuracion, entorno, client-side]
---

![logo svelte](/img/languages/svelte.png)

# Entorno

Es necesario que instales las siguientes herramientas, en este orden:

- Si estás en entorno Windows te recomendamos instalarte [Git Bash](https://gitforwindows.org/)
- Seguimos con [NodeJS](https://nodejs.org/en/).
  - Si estás en entorno Linux/Mac recomendamos que descargues Node desde [nvm (Node Version Manager)](https://github.com/nvm-sh/nvm) y luego instales esta versión: `nvm install 22.9.0`
  - Si estás en Windows instalate la versión actual
- Luego [NPM (Node Package Manager)](https://www.npmjs.com/), con el que vamos a hacer los builds de nuestras aplicaciones.
  - Para familiarizarte con el manejo de dependencias, te dejamos [este artículo](npm-dependencias.html)  

# Editor de Texto

## Visual Studio Code

- El editor de texto que vamos a soportar en la cursada es [**Visual Studio Code**](https://code.visualstudio.com/) (hay [una versión portable](https://sourceforge.net/projects/vscode-portable/) si estás en una máquina sin privilegios de administrador).

Dentro de Visual Studio Code, te recomendamos que crees un perfil vacío y lo asocies a tus nuevos proyectos Svelte (podés ver [cómo se trabaja con perfiles en VSCode en este video](https://www.youtube.com/watch?v=_2F2Zt-_tUA). También te dejamos [este tutorial muy piola - en inglés](https://www.youtube.com/watch?v=QjvvqR9KyVo) y [la documentación oficial](https://code.visualstudio.com/docs/editor/profiles)).

### Instalación 

> **Opción 1**: podés importar [este archivo que trae todas las extensiones para Svelte](https://github.com/algo3-unsam/proyecto-base-tp/blob/master/Svelte.code-profile)

O si no, podés instalar las extensiones del Visual Studio Code manualmente. Para 2024 son los que ya instalaste para trabajar con HTML/CSS y los siguientes:

### Necesarios ###

- **Auto Rename Tag (Jun Han)**: que permite acomodar la apertura y cierre de los tags de HTML
- **ESLint (Microsoft)**: para disparar el linter de la sintaxis de TS
- **Github Actions (Github)**: te ayuda con el archivo de configuración de CI para Github Actions
- **GitLens - Git Supercharged (GitKraken)**: utilidades para el trabajo con Git
- **Playwright Runner by Koushik (Koushik Chatterjee)**: ayuda a ejecutar tests e2e de Playwright
- **Playwright Snippets (Nitay Neeman)**: snippets de código que expanden tests e2e de Playwright
- **Playwright Test for VSCode (Microsoft)**: plugin para ejecutar tests e2e de Playwright
- **Svelte for VS Code (Svelte)**: plugin para soportar Svelte en VSCode
- **Vitest (Vitest)**: plugin para poder ejecutar los tests de frontend en el VSCode

### Opcionales ###

- **Import Cost (Wix)**: permite calcular cuántos KB pesa cada import
- **Material Icon Theme (Philipp Kief)**

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
│  ◼ prettier
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
- agregamos prettier, eslint, vitest y playwright. Vitest es para hacer tests de frontend y Playwright para tests e2e
- yarn como manejador de dependencias (es un poco más rápido que npm)

## Levantar la app

Para levantar la aplicación Svelte ejecutás

```bash
npm run dev
```

Luego en un navegador pedís la siguiente url: `http://localhost:5173`.

## Correr los tests de un proyecto

Para ejecutar los tests de un proyecto, te posicionás en el directorio raíz y ejecutás desde la consola

```bash
npm test
```
