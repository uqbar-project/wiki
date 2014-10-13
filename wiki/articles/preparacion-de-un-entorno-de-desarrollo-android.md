Download e instalación base
---------------------------

La instalación que nosotros recomendamos es la siguiente:

-   **Eclipse JEE Kepler**
    -   versiones anteriores **no funcionan**
    -   no hemos hecho pruebas con Eclipse Luna o posteriores
    -   URL de descarga: <http://www.eclipse.org/downloads/packages/eclipse-ide-java-ee-developers/keplersr2>

<!-- -->

-   Instalar **Android SDK** solamente, sin el Eclipse
    -   URL de descarga: <https://developer.android.com/sdk/index.html?hl=i>
    -   Pasos de instalación: GET THE SDK FOR AN EXISTING IDE &gt; Download the standalone SDK for ... sistema operativo que tenés ... &gt; Descomprimirlo a una carpeta ($Android\_SDK\_Path)

<!-- -->

-   Luego descargan el plugin de **Subclipse 1.10.x**
    -   Update site: <http://subclipse.tigris.org/update_1.10.x>
    -   Pasos de instalación: Help &gt; Install New Software &gt; seleccionan el update site y marcan ambos paquetes. Reinician el Eclipse una vez más.

<!-- -->

-   Luego descargan el **plugin de Android**
    -   Pasos de instalación: Help &gt; Eclipse Marketplace... &gt; en la solapa Search, buscan "Android". Seleccionan Android Development Tools for Eclipse, Install. Reinician el Eclipse una vez más.

<!-- -->

-   Luego de reiniciar el Eclipse JEE Kepler reapuntar el SDK al directorio donde se instaló: Window &gt; Preferences &gt; Android SDK Location: ubican ese directorio (el que estuvo en el paso $Android\_SDK\_Path). Abran el SDK Manager como les indica el Eclipse. Verifiquen si están instalados estos componentes:
    -   Android SDK Tools
    -   Android SDK Platform-Tools
    -   Android SDK Build-Tools

(la API recomendada es la 20).

Luego de esta instalación hay que cerrar el Android SDK Manager y volverlo a abrir porque no toma correctamente los cambios. Por si no lo encuentran el Android SDK Manager aparece desde el menú Window.

-   Abrir nuevamente el Android SDK Manager e instalar los **paquetes de los devices**.
    -   Android L (API 20, L preview)
        -   SDK Platform Android L Preview
    -   Android 4.4W (API 20)
        -   SDK Platform

Revisar el log, como son paquetes un tanto pesados es posible que haya que reintentar varias veces (cerrando y abriendo nuevamente el SDK Manager).

-   Desde el SDK Manager instalar los devices de la carpeta Android L (API 20, L preview)
    -   Android TV\*
    -   ARM EABI\*
    -   Intel x86\*

Tener paciencia, a veces no instala y hay que reintentar varias veces.

-   Configurar en el Android Virtual Device (AVD) Manager un **dispositivo para hacer las pruebas**. El AVD Manager aparece desde el menú Window. Les dejamos una opción posible:
    -   Nexus
    -   Device: Nexus One
    -   Target: Android L (Preview) - API Level L
    -   CPU/ABI: Intel Atom (x86\_64)
    -   Skin: HVGA permite manipular comandos más fácilmente (pero pueden probar con otros Skins, o incluso configurar otro dispositivo que tenga un Skin diferente, para simular tablets, o teléfonos con teclado)

Instalación del lenguaje Xtend para el entorno Android
------------------------------------------------------

-   En el eclipse, instalar el **plugin de Xtend**. Los ejemplos ya funcionan con la versión **2.7.2**.
    -   Update site: <http://download.eclipse.org/modeling/tmf/xtext/updates/composite/releases/>
    -   Pasos de instalación: Help &gt; Install New Software &gt; seleccionan el update site y luego marcan únicamente Xtend 2.7.2 para que no se vuelva muy pesado el entorno. Reinician el Eclipse.

### Configuración

-   Antes que nada chequeá las [Configuraciones generales para cualquier Eclipse](configuraciones-generales-para-cualquier-eclipse.html)
-   Window &gt; Preferences, Xtend compiler &gt; Output folder for generated Java files, cambiar el directorio a "gen"
-   En cada proyecto: agregar la librería Xtend en el proyecto: botón derecho sobre el proyecto &gt; Configure build path &gt; Libraries: Add Xtend Library, y luego chequear Xtend Library en la solapa "Order and Export" para que se incluya en el ejecutable de Android.

### Archetype de Maven para Android + Xtend

Si te interesa tenés un archetype para integrar estas tecnologías (disponible desde la versión 2.4.2 de Xtend):

`mvn archetype:generate -DarchetypeGroupId=org.eclipse.xtend \`
`  -DarchetypeArtifactId=xtend-android-archetype \`
`  -DarchetypeCatalog=`[`http://repo.maven.apache.org/maven2`](http://repo.maven.apache.org/maven2)

Troubleshooting
---------------

-   "could not find SDK folder" implica que no están apuntando al SDK que instalaron o bien que no instalaron el Android SDK.
    -   La solución: Window &gt; Preferences , filtran por Android y marcan el SDK Location que corresponda.

<!-- -->

-   Si al abrir una Activity de xtend se cierra misteriosamente el Eclipse, o luego no levanta, vayan a un archivo de log que generó en la carpeta donde corrieron el link al Eclipse, si hay un stack trace similar a éste:

`Java frames: (J=compiled Java code, j=interpreted, Vv=VM code)`
`j  org.eclipse.swt.internal.webkit.WebKitGTK._soup_session_feature_detach(JJ)V+0`

Significa que están corriendo un Eclipse anterior a Kepler.

-   -   La solución es bajarse un Eclipse Kepler y volver a seguir los pasos de instalación.

<!-- -->

-   Si al correr ven un mensaje de error que indica que no tienen devices, deben configurar un dispositivo para correr la aplicación Android (Android Virtual Device Manager del menú Window)

<!-- -->

-   Si cuando quieren configurar un device no les habilita el botón Ok porque dice "NO System images installed for this target", esto implica que falta descargar del Android SDK Manager las VM (imágenes) de los dispositivos que quieren emular.

<http://stackoverflow.com/questions/22541681/fail-to-create-android-virtual-device-no-system-image-installed-for-this-targe>

-   -   La solución: vayan entonces al Android SDK Manager y fíjense qué packages hay disponibles para instalar según la versión de Android que están ejecutando.

<!-- -->

-   Si al correr la aplicación aparece en el LogCat el siguiente mensaje:

`You can't combine swipe dismissal with ActionBar #1`

revisar si el device es Android Wear, porque no es compatible con los ejemplos.

-   Si al correr la aplicación Android aparece en el LogCat:

`"NoClassDefFoundError: Class "Lcom/google/common/base/Objects;" not found `

es porque olvidaron agregar la dependencia con xtend en el build path.

-   -   La solución: Botón derecho sobre el proyecto &gt; Build path &gt; Configure build path &gt; Order and Export &gt; y tildan el check Xtend Library.

<!-- -->

-   No hacer "Project clean" porque falla el compilador incremental de los archivos .xtend y se rompe todo el proyecto
    -   La solución es compilar manualmente los .xtend

Cómo empezar
------------

-   <http://developer.android.com/index.html>
-   <http://www.vogella.com/android.html>
-   Un [tutorial](http://www.youtube.com/watch?v=zS1frzHbKWY) de youtube que va bien tranquilo

### Material de Android en Xtend

-   <http://blog.efftinge.de/2011/12/writing-android-uis-with-xtend.html>
-   <http://nataliaossipova.wordpress.com/2013/03/10/xtending-android-development/>

### Herramientas de desarrollo

-   [Herramientas de desarrollo con Android](herramientas-de-desarrollo-con-android.html)

