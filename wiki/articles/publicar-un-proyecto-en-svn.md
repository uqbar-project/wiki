Desde el Eclipse:
-----------------

Botón derecho sobre el proyecto -&gt; Team -&gt; Share Project y ahí poner la url del repositorio donde esta el proyeto seguido de . Por ejemplo:

[`http://svn2.xp-dev.com/svn/nombre_del_proyecto/trunk`](http://svn2.xp-dev.com/svn/nombre_del_proyecto/trunk)`.`

Esto va a crear la carpeta trunk si no existe.

Este paso nos lleva a la perspectiva Synchronize del eclipse donde tenemos que comitear para guardar en el trunk nuestros sources del proyecto. En este momento, antes de comitear tenemos que indicarle al svn los archivos y carpetas que no queremos versionar, para esto, le damos botón derecho -&gt; Add to <svn:ignore>.

Después hay que crear las carpetas branches y tags.

Desde la consola:
-----------------
