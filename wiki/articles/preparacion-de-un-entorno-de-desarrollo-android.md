Download e instalación base
---------------------------

Descargá en [este link](http://www.eclipse.org/downloads/) el Eclipse IDE for Java EE Developers. Luego de [configurar tu entorno base](http://uqbar-wiki.org/index.php?title=Preparacion_de_un_entorno_de_desarrollo_Java), instalás el plugin para desarrollo de Android desde el Eclipse Marketplace.

Help &gt; Eclipse Marketplace... buscar "Android" Seleccionar Android Development Tool for Eclipse. Confirm Finish

Reiniciar el IDE.

Configuración para lenguaje Xtend
---------------------------------

Una vez instalado el entorno base

1.  Window &gt; Preferences, Xtend compiler &gt; generated folder cambiarlo a "gen"
2.  Agregar la librería Xtend en el proyecto: botón derecho sobre el proyecto &gt; Configure build path &gt; Libraries: Add Xtend Library, y luego chequear Xtend Library en la solapa "Order and Export" para que se incluya en el ejecutable de Android.

