La primera vez
--------------

Crear un repositorio SVN que apunte a la URL que te pasamos. **Ejemplo:** <http://xp-dev.com/svn/uqbar/examples/ui/web/wicket/xtend/>

Bajar el proyecto del SVN
-------------------------

En la perspectiva de SVN, vista de repositorios, navegar desde el directorio raíz hasta el proyecto. **Ejemplo:** contador-ui-wicket-xtend/trunk, y luego

-   Click derecho -&gt; Check out
-   Check out as a project in the workspace. En Project name escribir el nombre del proyecto correspondiente. Dejar las demás opciones por default.
-   Finish

Mavenizar el proyecto
---------------------

Botón derecho sobre el proyecto &gt; Configure &gt; Convert as Maven project o bien desde la consola

`mvn eclipse:eclipse`

Para proyectos xtend
--------------------

Ingresar a una clase xtend, modificarla (agregar un espacio y quitarlo) y luego grabarla. Deben generarse los .java correspondiente.

Incorporar la app al web server
-------------------------------

1.  Posicionarse en la solapa Servers: Window &gt; Show view &gt; Other... Server
2.  Luego botón derecho sobre Server &gt; Add and Remove...
3.  Pasar la aplicación del container Available a Configured (seleccionarla y luego presionar el botón Add&gt;)
4.  Finish
5.  Reiniciar el servidor (botón derecho &gt; Restart in Debug o Restart)

Acceder desde el browser a la direccion <http://localhost:8080/><aplicación web>/
