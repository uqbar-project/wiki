Guia de Instalación de Rails
============================

Introducción
------------

Ruby On Rails es, entre otras cosas, un framework de presentación Web MVC, que utiliza el lenguaje de programación Ruby. Para instalarlo, hay que instalar primero Ruby, algunas herramientas de desarrollo estándar de esta tecnología, y finalmente, Rails.

Antes de empezar
----------------

-   Este tutorial asume que ya seguiste la [Guía de Instalación de Ruby](guia-de-instalacion-de-ruby.html)
-   Este tutorial asume que estás trabajando en un entorno Linux. Si bien Ruby funciona en Windows, no se recomienda usar Rails en este sistema operativo.

Pasos
-----

1.  Instalar Rails

`gem install rails`

1.  Crear un Proyecto

`rails new `<NombreDelProyecto>
`cd `<NombreDelProyecto>

1.  Configurar la versión de Ruby que se esté usando, por ejemplo:

`rbenv local 2.0.0-p481`

Problemas Frecuentes
--------------------

-   No usar apt-get para instalar
-   Falta el runtime de JS

Más información
---------------

<https://gorails.com/setup/ubuntu/14.04>
