Desde el Eclipse:
-----------------

Botón derecho sobre el proyecto -&gt; Team -&gt; Share Project y ahí poner la url del repositorio donde esta el proyeto seguido de . Por ejemplo:

[`http://svn2.xp-dev.com/svn/nombre_del_proyecto/trunk`](http://svn2.xp-dev.com/svn/nombre_del_proyecto/trunk)`.`

Esto va a crear la carpeta trunk si no existe.

Este paso nos lleva a la perspectiva Synchronize del eclipse donde tenemos que comitear para guardar en el trunk nuestros sources del proyecto. En este momento, antes de comitear tenemos que indicarle al svn los archivos y carpetas que no queremos versionar, para esto, le damos botón derecho -&gt; Add to <svn:ignore>.

Lo que tenemos que agregar al <svn:ignore> en un proyecto simple es:

-   la carpeta .settings que guarda información del eclipse
-   la carpeta target que es donde estan los compilados
-   el y el

Recordemos que siempre ponemos en el <svn:ignore> todo aquello que se genera automáticamente, en este caso, todos estos archivos los genera el maven.

Después hay que crear las carpetas branches y tags.

Desde consola:
--------------

Desde la consola:
-----------------
