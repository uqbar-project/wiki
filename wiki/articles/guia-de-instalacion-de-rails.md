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

### Instalar Rails

`gem install rails`

### Crear un Proyecto

`rails new `<NombreDelProyecto>
`cd `<NombreDelProyecto>

### Configurar la versión de Ruby

Por ejemplo:

`rbenv local 2.0.0-p481`

Problemas Frecuentes
--------------------

### No se puede instalar SQLITE3

Si tienen un error de este estilo:

`Gem::Ext::BuildError: ERROR: Failed to build gem native extension.`

`/usr/bin/ruby1.9.1 -r ./siteconf20140902-24182-zuispu.rb extconf.rb `
`` /usr/local/lib/site_ruby/1.9.1/rubygems/core_ext/kernel_require.rb:54:in `require': cannot load such file -- mkmf (LoadError) ``

` An error occurred while installing sqlite3 (1.3.9), and Bundler cannot continue.`
``  Make sure that `gem install sqlite3 -v '1.3.9'` succeeds before bundling. ``

Verifiquen que instalaron ruby usando rbenv, y rbenv usando rbenv-installer, como explica [Guia de Instalación de Ruby](guia-de-instalacion-de-ruby.html). No usen apt-get para instalar ninguno de estos componentes

### No se puede instalar libv8

Si tienen un error de este estilo:

` ERROR:  Error installing libv8:`
`   ERROR: Failed to build gem native extension.`
`       /usr/bin/ruby1.9.1 extconf.rb`
``  /usr/lib/ruby/1.9.1/rubygems/custom_require.rb:36:in `require': cannot load such file -- mkmf (LoadError) ``
``    from /usr/lib/ruby/1.9.1/rubygems/custom_require.rb:36:in `require' ``
``    from extconf.rb:1:in ` ``<main>`'`
`  Gem files will remain installed in /var/lib/gems/1.9.1/gems/libv8-3.11.8.0 for inspection.`
`  Results logged to /var/lib/gems/1.9.1/gems/libv8-3.11.8.0/ext/libv8/gem_make.out`

Verifiquen lo mismo que en el título anterior.

### Falta el runtime de JS

Si al intentar levantar el servidor con

`rails server`

Les dice que falta un "JavaScript Runtime", descomenten en en Gemfile la línea que dice

`gem 'therubyracer'`

Más información
---------------

<https://gorails.com/setup/ubuntu/14.04>
