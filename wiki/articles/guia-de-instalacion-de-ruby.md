---
layout: article
title: Guia de instalacion de ruby
---

Guía de Instalación de Ruby
===========================

Introducción
------------

La siguiente es una guía de instalación de Ruby utilizando un gestión de versiones (rbenv). Si bien este no es estrictamente necesario, simplifica el proceso de instalación, y es además fundamental cuando se desea trabajar con múltiples versiones de Ruby, algo bastante común cuando se está trabajando en más de un proyecto Ruby en una misma computadora.

Esta guía además instala algunas dependencias que en verdad son sólo necesarias para el framework Rails, pero de todas formas las incluimos porque son livianas, fáciles de instalar, y previenen problemas futuros.

Pasos
-----

### 1. Instalar essentials

`sudo apt-get install curl git build-essential libssl-dev autoconf bison libreadline6 libreadline6-dev zlib1g zlib1g-dev libsqlite3-dev sqlite3`

Explicación: varias herramientas y bibliotecas de Ruby necesitan bajar contenido de Internet, y compilar código nativo.

### 2. Instalar RBENV

Seguir las instrucciones en el [repositorio de github de rbenv-installer](https://github.com/rbenv/rbenv-installer). El resumen es:

`curl `[`https://github.com/rbenv/rbenv-installer`](https://github.com/rbenv/rbenv-installer)` | bash`
`echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bash_profile`
`echo 'eval "$(rbenv init -)"' >> ~/.bash_profile`

Explicación: Ruby suele cambiar bastante entre versiones, lo cual genera incompatibilidades. Para no tener problemas, se recomienda instalar Ruby a través de un manejador de versiones, que además de simplificar el proceso, permite usar versiones diferentes de esta tecnología en cada proyecto.

Nota: Si usan Ubuntu y lo anterior no funciona revisen estas instrucciones <https://github.com/sstephenson/rbenv#installation>

### 3. Verificar que se haya instalado rbenv

`echo $PATH `

La linea ~/.rbenv/bin tiene que estar presente. Si no lo está, asegúrense de que el código del punto 2 se agregó correctamente en .bashrc o .bash\_profile, y de que hayan reiniciado la terminal.

### 4. Instalar una versión de Ruby y dejarlo como opción por defecto

`rbenv install 2.0.0-p481`
`rbenv global 2.0.0-p481`
`rbenv rehash`

Explicación: con esto instalamos una versión concreta de Ruby, y la dejamos lista para ser utilizada.

### 5. Instalar Bundler

`gem install bundler`

Explicación: Bundler es una herramienta, que al igual que Maven, permite gestionar las dependencias de un proyecto.

Problemas frecuentes
--------------------

-   No se reconoce el comando rbenv: asegúrense de que hayan reiniciado la terminal, y que hayan colocado el código de inicialización en el archivo correcto (.bashrc o .bash\_profile, según qué Linux usen)

