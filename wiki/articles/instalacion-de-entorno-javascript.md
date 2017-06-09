---
layout: article
title: Instalacion de entorno javascript
---

(Solo para Windows) Git Bash
----------------------------
Si tenés una instalación de Windows te recomendamos previamente descargar [Git Bash](https://git-for-windows.github.io/)


Manejo de dependencias, automatización de tareas
------------------------------------------------

Para ésta y otras tareas, vamos a utilizar la plataforma [Node](https://nodejs.org/), que también es un servidor web desarrollado en javascript.

En Linux podés hacer

``` bash
$ sudo apt-get install nodejs
```

o seguir las instrucciones oficiales que están [aquí](https://github.com/nodejs/node/wiki).

Para cualquier sistema operativo en general podés descargar Node en <https://nodejs.org/download/>

Entorno de desarrollo
---------------------

Las herramientas para esta tecnología todavía no están tan maduras, si estuviste trabajando en Eclipse o IntelliJ posiblemente vas a extrañar los refactors, la detección de imports automáticos, los quick fixes y muchas otras cosas que hacían más feliz el trabajo del día a día. No obstante hay entornos que van sumando funcionalidades y mejoras año tras año. 

### Visual Studio Code (recomendado)

El **[Visual Studio Code](https://code.visualstudio.com/)** es un editor de texto basado en Sublime que cuenta con

- linter o chequeo de sintaxis
- autocompletado inteligente de tipos
- herramientas de debugging
- integración con git
- soporte para ES6
  
Seguí los pasos de instalación que propone el sitio oficial.


### Plugins de javascript para Eclipse

Si te sentís cómodo con Eclipse podés incorporarle los plugins de javascript aunque las funcionalidades en este entorno están bastante más limitadas. A continuación te explicamos cómo agregar los plugins para trabajar con javascript:

-   Ir a "Help-&gt;Install New Software..."
-   Buscar en el combo la URL de nuestra distribución de eclipse (Por ejemplo con eclipse Neon es <http://download.eclipse.org/releases/neon>)
-   Buscar los items:
    -   Eclipse Web Developer Tool
    -   Eclipse Xml Editors and Tool, y
    -   JavaScript Development Tools

### Otros editores de texto

- [**Sublime 3**](https://www.sublimetext.com/), al que luego tenés que agregarle paquetes para incorporarle funcionalidades
    - Si trabajás en ES6 chequeá cómo instalar [Intellisense](https://medium.com/beyond-the-manifesto/configuring-sublime-text-3-for-modern-es6-js-projects-6f3fd69e95de) y el [Linter (que hace los chequeos)](http://jonathancreamer.com/setup-eslint-with-es6-in-sublime-text/), que en definitiva es llegar al mismo paso que con Visual Studio Code
-   [**Web Storm**](https://www.jetbrains.com/webstorm/): posiblemente sea el mejor IDE al momento, tiene la contra de ser una herramienta paga aunque podés pedir licencias educativas
-   [**Brackets**](http://brackets.io/)
-   [**Atom.io**](https://atom.io/)



Framework de testeo unitario
----------------------------

Para hacer testeo unitario tenés varias opciones

### Jasmine (recomendado)

-   [Jasmine](http://jasmine.github.io/), que es la opción que te recomendamos. Para probarlo en el cliente, basta con descargar en <https://github.com/jasmine/jasmine/releases> el proyecto web standalone (**jasmine-standalone-x.y.z.zip** donde x, y, z es el último número de versión), integrarlo dentro de tu proyecto Javascript existente y luego editar el archivo SpecRunner.html, incorporando los archivos js de dominio y de test que quieras. Todo esto y bastante más está explicado en <https://jasmine.github.io/2.6/introduction> (o bueno, podés navegar el sitio y acceder a la última versión de la documentación)

### Otras opciones

Te dejamos alternativas a Jasmine

-   [Mocha](http://mochajs.org/)
-   [Cucumber](https://github.com/cucumber/cucumber-js)
-   especializados en ReactJS, tenés [Enzyme](http://airbnb.io/enzyme/) y [Jest](https://facebook.github.io/jest/)
-   [Ava](https://github.com/avajs/ava)
-   Pueden ver un [interesante artículo sobre frameworks de testeo unitario](http://stateofjs.com/2016/testing/)

