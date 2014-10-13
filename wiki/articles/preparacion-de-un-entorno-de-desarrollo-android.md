Download e instalación base
---------------------------

La instalación que nosotros recomendamos es la siguiente:

-   Eclipse JEE Kepler
    -   versiones anteriores **no funcionan**
    -   no hemos hecho pruebas con Eclipse Luna o posteriores
    -   URL de descarga: <http://www.eclipse.org/downloads/packages/eclipse-ide-java-ee-developers/keplersr2>

<!-- -->

-   Instalar Android SDK for Linux solamente, sin el Eclipse
    -   URL de descarga: <https://developer.android.com/sdk/index.html?hl=i>
    -   Pasos de instalación: GET THE SDK FOR AN EXISTING IDE &gt; Download the standalone SDK for ... sistema operativo que tenés ... &gt; Descomprimirlo a una carpeta ($Android\_SDK\_Path)

<!-- -->

-   En el eclipse, instalar el plugin de Xtend
    -   los ejemplos ya funcionan con la versión 2.7.2
    -   Update site: <http://download.eclipse.org/modeling/tmf/xtext/updates/composite/releases/>
    -   Pasos de instalación: Help &gt; Install New Software &gt; seleccionan el update site y luego marcan únicamente Xtend 2.7.2 para que no se vuelva muy pesado el entorno. Reinician el Eclipse.

<!-- -->

-   Luego descargan el plugin de Subclipse 1.10.x
    -   Update site: <http://subclipse.tigris.org/update_1.10.x>
    -   Pasos de instalación: Help &gt; Install New Software &gt; seleccionan el update site y marcan ambos paquetes. Reinician el Eclipse una vez más.

<!-- -->

-   Luego descargan el plugin de Android
    -   Pasos de instalación: Help &gt; Eclipse Marketplace... &gt; en la solapa Search, buscan "Android". Seleccionan Android Development Tools for Eclipse, Install. Reinician el Eclipse una vez más.

<!-- -->

-   Luego de reiniciar el Eclipse JEE Kepler reapuntar el SDK al directorio donde se instaló: Window &gt; Preferences &gt; Android SDK Location: ubican ese directorio (el que estuvo en el paso $Android\_SDK\_Path). Abran el SDK Manager como les indica el Eclipse. Verifiquen si están instalados estos componentes:
    -   Android SDK Tools
    -   Android SDK Platform-Tools
    -   Android SDK Build-Tools

(la API recomendada es la 20).

Luego de esta instalación hay que cerrar el Android SDK Manager y volverlo a abrir porque no toma correctamente los cambios. Por si no lo encuentran el Android SDK Manager aparece desde el menú Window.

-   Abrir nuevamente el Android SDK Manager e instalar los paquetes de los devices.
    -   Android L (API 20, L preview)
        -   SDK Platform Android L Preview
    -   Android 4.4W (API 20)
        -   SDK Platform

Revisar el log, como son paquetes un tanto pesados es posible que haya que reintentar varias veces (cerrando y abriendo nuevamente el SDK Manager).

-   Desde el SDK Manager instalar los devices de la carpeta Android L (API 20, L preview)
    -   Android TV\*
    -   ARM EABI\*
    -   Intel x86\*
-   de la carpeta Android 4.4W (API 20)
    -   Android Wear\*

Tener paciencia, a veces no instala y hay que reintentar varias veces.

-   Configurar en el Android Virtual Device (AVD) Manager un dispositivo para hacer las pruebas. El AVD Manager aparece desde el menú Window. Les dejamos una opción posibel:
    -   Nexus
    -   Device: Nexus One
    -   Target: Android L (Preview) - API Level L
    -   CPU/ABI: Intel Atom (x86\_64)
    -   Skin: HVGA permite manipular comandos más fácilmente (pero pueden probar con otros Skins, o incluso configurar otro dispositivo que tenga un Skin diferente, para simular tablets, o teléfonos con teclado)

Descargá en [este link](http://www.eclipse.org/downloads/) el Eclipse IDE for Java EE Developers. Luego de [configurar tu entorno base](http://uqbar-wiki.org/index.php?title=Preparacion_de_un_entorno_de_desarrollo_Java), instalás el plugin para desarrollo de Android desde el Eclipse Marketplace.

1.  Help &gt; Eclipse Marketplace...
2.  buscar "Android"
3.  Seleccionar Android Development Tool for Eclipse.
4.  Confirm
5.  Finish

Reiniciar el IDE.

### Cómo empezar

-   <http://developer.android.com/index.html>
-   <http://www.vogella.com/android.html>
-   Un [tutorial](http://www.youtube.com/watch?v=zS1frzHbKWY) de youtube que va bien tranquilo

Configuración para lenguaje Xtend
---------------------------------

Una vez instalado [el entorno base](preparacion-de-un-entorno-de-desarrollo-xtend.html)

1.  Window &gt; Preferences, Xtend compiler &gt; Output folder for generated Java files, cambiar el directorio a "gen"
2.  En cada proyecto: agregar la librería Xtend en el proyecto: botón derecho sobre el proyecto &gt; Configure build path &gt; Libraries: Add Xtend Library, y luego chequear Xtend Library en la solapa "Order and Export" para que se incluya en el ejecutable de Android.

### Archetype de Maven para Android + Xtend

Si te interesa tenés un archetype para integrar estas tecnologías (disponible desde la versión 2.4.2 de Xtend):

`mvn archetype:generate -DarchetypeGroupId=org.eclipse.xtend \`
`  -DarchetypeArtifactId=xtend-android-archetype \`
`  -DarchetypeCatalog=`[`http://repo.maven.apache.org/maven2`](http://repo.maven.apache.org/maven2)

Más información en <http://www.eclipse.org/xtend/releasenotes_2_4.html>

### Material de Android en Xtend

-   <http://blog.efftinge.de/2011/12/writing-android-uis-with-xtend.html>
-   <http://nataliaossipova.wordpress.com/2013/03/10/xtending-android-development/>

### Herramientas de desarrollo

-   [Herramientas de desarrollo con Android](herramientas-de-desarrollo-con-android.html)

