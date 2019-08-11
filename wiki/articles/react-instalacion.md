---
layout: article
title: Instalacion de arena
categories: [arena, entorno, instalacion]
featured: true
---

<img src="/img/languages/uqbar-arena-logo.png" height="30%" width="30%"/>

## Pasos previos

Si ya estuviste trabajando con Angular estos pasos no son necesarios, pero conviene verificar que ya estén instalados.

- Si estás en entorno Windows te recomendamos instalarte [Git Bash](https://gitforwindows.org/)
- Seguimos con [NodeJS](https://nodejs.org/en/), preferentemente la última versión.
- Luego [NPM (Node Package Manager)](https://www.npmjs.com/), con el que vamos a hacer los builds de nuestras aplicaciones.
- El editor de texto que vamos a soportar en la cursada es [Visual Studio Code](https://code.visualstudio.com/) (hay una versión portable si estás en una máquina sin privilegios de administrador).

## Específicos de React

Dentro de Visual Studio Code, los plugins que recomendamos para trabajar con React son:

- **ESLint**: dbaeumer.vscode-eslint. En todos los proyectos deberías tener en el directorio raíz el archivo .eslintrc.json con la siguiente configuración

```json
{
    "parser": "babel-eslint",
    "extends": "",
    "plugins": [
        "react"
    ],
    "parserOptions": {
        "ecmaVersion": 6,
        "sourceType": "module",
        "ecmaFeatures": {
            "jsx": true
        }
    }
}
```

- VS Code ES7 React/Redux/React-Native/JS snippets, o cualquier conjunto de comandos rápidos para Visualt Studio Code
- npx, que se instala con npm

```bash
npm install -g npx
```

## Crear un proyecto React de cero

Para crear un proyecto React desde la consola Git Bash o bien desde una terminal Linux escribimos:

```bash
npx create-react-app nombre-de-tu-app
cd nombre-de-tu-app
npm start
```

Por defecto la aplicación cliente levantará en el puerto 3000. Como suele quedarse levantada aun cuando canceles la línea de comando y el navegador, te dejamos este link que te dice [cómo bajar el proceso del sistema operativo](https://stackoverflow.com/questions/39322089/node-js-port-3000-already-in-use-but-it-actually-isnt) para correr otro ejemplo.

## Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
