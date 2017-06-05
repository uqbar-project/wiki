---
layout: article
title: Instalacion de entorno javascript
---

Manejo de dependencias, automatización de tareas
------------------------------------------------

Para ésta y otras tareas, vamos a utilizar la plataforma Node, que también es un servidor web desarrollado en javascript.

En Linux podés hacer

``` bash
$ sudo apt-get install nodejs
```

o seguir las instrucciones oficiales que están [aquí](https://github.com/nodejs/node/wiki).

- en Windows te recomendamos previamente descargar el Git Bash en <http://msysgit.github.io/>
- y para cualquier sistema operativo en general podés descargar Node en <https://nodejs.org/download/>

Framework de testeo unitario
----------------------------

Para hacer testeo unitario tenés varias opciones

### Jasmine (recomendado)

-   [Jasmine](http://jasmine.github.io/), que es la opción que te recomendamos. Para probarlo en el cliente, basta con descargar en <https://github.com/jasmine/jasmine/releases> el proyecto web standalone (**jasmine-standalone-x.y.z.zip** donde x, y, z es el último número de versión), integrarlo dentro de tu proyecto Javascript existente y luego editar el archivo SpecRunner.html, incorporando los archivos js de dominio y de test que quieras. Todo esto y bastante más está explicado en <http://jasmine.github.io/2.5/introduction.html> (o bueno, podemos ver la última versión)

### Mocha

Tambien podés descargarte Mocha.js como framework de testeo unitario para tus proyectos, haciendo

``` bash
$ npm install -g mocha
```

npm es node package manager, por lo que tenés que haber instalado node primero. Si estás en Windows correlo desde el Git Bash que instalaste previamente.

Para más información entrá a <http://mochajs.org/>

Entorno de desarrollo
---------------------

-   [**Sublime 3**](https://www.sublimetext.com/) (opción recomendada): *no es un IDE* y vas a extrañar los menúes de refactor, los imports, y muchas cosas que venían con Eclipse. Pero es bastante liviano, tiene funciones de autocompletado y más abajo te comentamos qué plugins (o package controls) se le pueden instalar para que tu estadía en javascript sea más placentera.
-   [**Web Storm**](https://www.jetbrains.com/webstorm/): posiblemente sea el mejor IDE al momento, tiene la contra de ser una herramienta paga aunque podés pedir licencias educativas
-   **Eclipse + plugins**: si te sentís cómodo con Eclipse podés incorporarle los plugins de javascript (más abajo te explicamos cómo). De todas maneras no tenés demasiadas funcionalidades dentro de este entorno, olvidate de los refactors inteligentes que tenías si trabajabas en Java.
-   [**Brackets**](http://brackets.io/)
-   [**Atom.io**](https://atom.io/)

### Sublime

-   [Link a la página principal](http://www.sublimetext.com/3) donde te descargás el producto
-   Instalá el Package Control, la herramienta que necesitás para instalar los plugins. Tenés [este tutorial](https://packagecontrol.io/installation) que explica cómo bajarlo desde Sublime.

Hay que descargarse el Linter vía npm:

``` bash
$ sudo npm install -g jshint
```

(si estás en Windows iniciá la consola como Administrador y ejecutá directamente npm install -g jshint sin sudo).

Luego te recomendamos que descargues estos packages controls:

-   SublimeLinter
-   SublimeLinter-jshint
-   SublimeLinter-json

La documentación de cómo instalarlo está en: <http://www.sublimelinter.com/en/latest/installation.html>

Estos 3 paquetes marcan las líneas donde hay errores con un puntito y subrayan el error en el código mismo, además de que en la barra de estado te describe el error.

-   **HTML-CSS-JS Prettify**: ctrl + shift + h y te acomoda el código
-   **Color Highlighter:** podés ver de qué color quedan los colores que ponés codificados en el código (por nombre, rgb, rgba, etc.)
-   **DocBlockr:** si empezás un comentario, te lo sigue en la siguiente línea si le das enter (podés evitarlo con shift + enter)

Para levantar un Browser desde el Sublime cuando estés parado en un HTML: <http://michaelcrump.net/getting-sublime-3-to-launch-your-html-page-in-a-browser-with-a-key-combo/>

### Eclipse

Para instalarse los plugins de Eclipse tenés que configurar tu entorno básico según se explica en la página [Preparacion de un entorno de desarrollo Java](preparacion-de-un-entorno-de-desarrollo-java.html). Luego:

-   Ir a "Help-&gt;Install New Software..."
-   Buscar en el combo la URL de nuestra distribución de eclipse (Por ejemplo con eclipse Juno es "<http://download.eclipse.org/releases/juno>")
-   Buscar de entre todo el software los items:
    -   "Eclipse Web Developer Tool"
    -   "Eclipse Xml Editors and Tool", y
    -   "JavaScript Development Tools"

