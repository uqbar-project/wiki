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

Framework de testeo unitario
----------------------------

Para hacer testeo unitario tenés varias opciones

### Jasmine (recomendado)

-   [Jasmine](http://jasmine.github.io/), que es la opción que te recomendamos. Para probarlo en el cliente, basta con descargar en <https://github.com/jasmine/jasmine/releases> el proyecto web standalone (**jasmine-standalone-x.y.z.zip** donde x, y, z es el último número de versión), integrarlo dentro de tu proyecto Javascript existente y luego editar el archivo SpecRunner.html, incorporando los archivos js de dominio y de test que quieras. Todo esto y bastante más está explicado en <https://jasmine.github.io/2.6/introduction> (o bueno, podés navegar el sitio y acceder a la última versión de la documentación)

### Mocha

Tambien podés descargarte Mocha.js como framework de testeo unitario para tus proyectos, ejecutando por consola en cualquier sistema operativo

``` bash
$ npm install -g mocha
```

npm es node package manager, por lo que tenés que haber instalado node primero. Si estás en Windows correlo desde el Git Bash que instalaste previamente.

Para más información entrá a <http://mochajs.org/>

Entorno de desarrollo
---------------------

### Sublime (recomendado)

El [**Sublime 3**](https://www.sublimetext.com/) *no es un IDE* y vas a extrañar los menúes de refactor, los imports, y muchas cosas que venían con Eclipse. Pero es bastante liviano, tiene funciones de autocompletado y más abajo te comentamos qué plugins (o package controls) se le pueden instalar para que tu estadía en javascript sea más placentera.

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

Para levantar un Browser desde el Sublime cuando estés parado en un HTML seguí [estas instrucciones](<http://michaelcrump.net/getting-sublime-3-to-launch-your-html-page-in-a-browser-with-a-key-combo/>)

### Plugins de javascript para Eclipse

Si te sentís cómodo con Eclipse podés incorporarle los plugins de javascript aunque las funcionalidades en este entorno están bastante más limitadas. A continuación te explicamos cómo agregar los plugins para trabajar con javascript:
-   Ir a "Help-&gt;Install New Software..."
-   Buscar en el combo la URL de nuestra distribución de eclipse (Por ejemplo con eclipse Neon es "<http://download.eclipse.org/releases/neon>")
-   Buscar de entre todo el software los items:
    -   "Eclipse Web Developer Tool"
    -   "Eclipse Xml Editors and Tool", y
    -   "JavaScript Development Tools"

### Otros editores de texto

-   [**Web Storm**](https://www.jetbrains.com/webstorm/): posiblemente sea el mejor IDE al momento, tiene la contra de ser una herramienta paga aunque podés pedir licencias educativas
-   [**Brackets**](http://brackets.io/)
-   [**Atom.io**](https://atom.io/)

