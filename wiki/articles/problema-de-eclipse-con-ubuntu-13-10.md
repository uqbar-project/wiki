Problema de Eclipse con Ubuntu 13.10
------------------------------------

Si instalaste Eclipse en Ubuntu 13.10 y además utilizás el entorno de escritorio Unity, es posible que tengas problemas a la hora de visualizar los menús (file, edit, view, etc). Para arreglar este problema, hay que editar a mano unos archivos tal como se indica en esta solución.

O también podes actualizar tu sistema operativo, a partir de Ubuntu 14.04 LTS el problema ya fue corregido.

Este mismo problema también se da con otras aplicaciones (gimp, inkscape, ...)

Solución
--------

Hay que buscar los archivos eclipse.desktop en las rutas /usr/share/applications y ~/.local/share/applications/ y luego editarlos con cualquier editor de texto (nano, gedit, etc). En caso de que hayas descargado eclipse desde la página, es posible que no tengas el archivo eclipse.desktop, en tal caso deberás crearlo.

-   Para esto, debemos primero abrir la terminal (ctrl + alt + t) y pararnos en cada ruta, utilizando el comando cd.

`      cd /usr/share/applications`

-   Luego, debemos utilizar un editor de texto para cambiar la línea que queremos cambiar. Si no tenías el archivo eclipse.desktop este comando también te lo crea de ser necesario.

`      sudo nano eclipse.desktop`

Y reemplazamos la línea que comienza con Exec. Si descargaste Eclipse desde la página entonces:

`      Exec=env UBUNTU_MENUPROXY= /ruta/al/eclipse/eclipse`

Por ejemplo: /home/TU\_USUARIO/Escritorio/DDS/eclipse

Si instalaste eclipse desde los repositorios ppa, entonces eclipse debe encontrarse instalado en /usr/bin/, en tal caso no es necesario poner la ruta, sino simplemente reemplazar por

`      Exec=env UBUNTU_MENUPROXY= eclipse`

-   Luego repetimos los pasos anteriores para el eclipse.desktop que se encuentra en la ruta ~/.local/share/applications/.

<!-- -->

-   Reiniciamos el eclipse y los menús deberían poder visualizarse correctamente.

Si seguiste los pasos adecuadamente, el archivo debería quedar parecido a este: (si lo instalaste desde los repositorios ppa)

`      [Desktop Entry]`
`      Type=Application`
`      Name=Eclipse`
`      Comment=Eclipse Integrated Development Environment`
`      Icon=eclipse`
`      Exec=env UBUNTU_MENUPROXY= eclipse`
`      Terminal=false`
`      Categories=Development;IDE;Java;`
