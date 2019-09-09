---
layout: article
title: Instalacion de entorno javascript
categories: [javascript, es6, entorno, ecmascript, instalacion]
featured: true
---

<img src="/img/languages/ES6-ecmascript6-logo.jpg" width="30%" height="30%"/>

# Prerrequisitos (Solo para Windows) Git Bash

Si tenés una instalación de Windows te recomendamos previamente descargar [Git Bash](https://git-for-windows.github.io/)

# Manejo de dependencias, automatización de tareas

Para ésta y otras tareas, vamos a utilizar la plataforma [Node](https://nodejs.org/), que también es un servidor web desarrollado en javascript.

En Linux podés hacer

``` bash
$ sudo apt-get install nodejs
```

o seguir las instrucciones oficiales que están [aquí](https://github.com/nodejs/node/wiki). Para cualquier sistema operativo en general podés descargar Node en <https://nodejs.org/download/>

# Entorno de desarrollo

Las herramientas para esta tecnología todavía no están tan maduras, si estuviste trabajando en Eclipse o IntelliJ posiblemente vas a extrañar los refactors, la detección de imports automáticos, los quick fixes y muchas otras cosas que hacían más feliz el trabajo del día a día. No obstante hay entornos que van sumando funcionalidades y mejoras año tras año.

## Visual Studio Code (recomendado)

El **[Visual Studio Code](https://code.visualstudio.com/)** es un editor de texto basado en Sublime que cuenta con

- linter o chequeo de sintaxis
- autocompletado inteligente de tipos
- herramientas de debugging
- integración con git
- soporte para ES6
  
Seguí los pasos de instalación que propone el sitio oficial.

Una vez instalado, te recomendamos que actives estas extensiones (Ctrl + Shift + X):

![image](/img/wiki/extensionesSublime.png)

- **ESLint** (muy importante): valida el código ES6 que escribas (integrado con el editor)
- **Eclipse Keymap**: para tener los mismos shortcuts del Eclipse
- **Beautify**: Permite formatear el código adecuadamente
- **HTML Class suggestions**: para ofrecer autocomplete en el HTML de los class que tu css define.
- **Debugger for Chrome**: Integra el debugger de Chrome en el editor
- **npm**: integra la herramienta npm con el editor
- **npm Intellisense**: autocompletado para ejecutar instrucciones desde el npm
- EditorConfig for VSCode
- **Git project manager**: logra una integración entre el editor y los comandos de git
- Git History (git log)

El ESLint requiere una instalación mediante npm:

```bash
# npm install -g eslint
```

## Otras opciones

- [**Web Storm**](https://www.jetbrains.com/webstorm/): posiblemente sea el mejor IDE al momento, tiene la contra de ser una herramienta paga aunque podés pedir licencias educativas
- Si vas a elegir otro editor de texto como Atom o Sublime, chequeá qué actualizaciones tiene para ES6.

# Framework de testeo unitario

Para hacer testeo unitario tenés varias opciones

## Jasmine (recomendado)

- [Jasmine](http://jasmine.github.io/), que es la opción que te recomendamos. Para probarlo en el cliente, basta con descargar en <https://github.com/jasmine/jasmine/releases> el proyecto web standalone (**jasmine-standalone-x.y.z.zip** donde x, y, z es el último número de versión), integrarlo dentro de tu proyecto Javascript existente y luego editar el archivo SpecRunner.html, incorporando los archivos js de dominio y de test que quieras. Todo esto y bastante más está explicado en <https://jasmine.github.io/2.6/introduction> (o bueno, podés navegar el sitio y acceder a la última versión de la documentación)

## Otras opciones

Te dejamos alternativas a Jasmine

- [Mocha](http://mochajs.org/)
- [Cucumber](https://github.com/cucumber/cucumber-js)
- especializados en ReactJS, tenés [Enzyme](http://airbnb.io/enzyme/) y [Jest](https://facebook.github.io/jest/)
- [Ava](https://github.com/avajs/ava)

# Links relacionados

- [Primeros pasos con javascript - ES6](primeros-pasos-con-es6.html)
- [Temario Algoritmos III](algo3-temario.html)
