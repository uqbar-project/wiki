Download e instalación base
---------------------------

La instalación que nosotros recomendamos es la siguiente:

-   **Android SDK**
    -   URL de descarga: <https://developer.android.com/sdk/index.html?hl=i>
    -   Pasos de instalación: GET THE SDK FOR AN EXISTING IDE &gt; Download the standalone SDK for ... sistema operativo que tenés ... &gt; Descomprimirlo a una carpeta ($Android\_SDK\_Path)

<!-- -->

-   **Android Studio**
    -   <https://developer.android.com/sdk/index.html> (descargar última versión que a Noviembre 2015 es Marshmallow)

Las configuraciones que te recomendamos son:

-   Si estás familiarizado con los shortcuts del Eclipse: File &gt; Settings &gt; Keymap y en el combo Keymaps seleccionar Eclipse
-   plugin de **Subclipse 1.10.x**??

<!-- -->

-   Luego reapuntar el SDK al directorio donde se instaló: Tools &gt; Android &gt; SDK Manager: ubican ese directorio Android SDK Location (el que estuvo en el paso $Android\_SDK\_Path). Además deben tener instalada alguna API (API Level xx, Revision x, Status: deben marcarla y luego seleccionar Apply). Una vez instalado reiniciar.

<!-- -->

-   Para hacer las pruebas tienen dos opciones: utilizar un dispositivo Android conectado a USB o bien configurar un emulador mediante el Android Virtual Device (AVD). El AVD Manager aparece desde el menú Tools &gt; Android &gt; AVD Manager. Les dejamos una opción posible:
    -   AVD Name: El nombre que quieran
    -   Device: 5.0" 1080x1920 420dpi
    -   Lollipop: Android 5.1 x86\_64
    -   Scale: Auto
    -   Emulated Performance: Use Host GPU chequeado, Store a snapshot for faster startup deschequeado

### Configuración

-   Antes que nada chequeá las [Configuraciones generales para cualquier Eclipse](configuraciones-generales-para-cualquier-eclipse.html)

Troubleshooting
---------------

### "could not find SDK folder"

Implica que no están apuntando al SDK que instalaron o bien que no instalaron el Android SDK. Solución: Tools &gt; Android &gt; SDK Manager, filtran por Android y marcan el SDK Location que corresponda.

### No se puede ejecutar una app: "no devices"

Si al correr ven un mensaje de error que indica que no tienen devices, deben configurar un dispositivo para correr la aplicación Android (Android Virtual Device Manager del menú Window) o bien utilizar un dispositivo real conectado a USB.

### "NO System images installed for this target"

Si aparece ese error cuando quieren configurar un device y no les habilita el botón Ok, esto implica que falta descargar del Android SDK Manager las VM (imágenes) de los dispositivos que quieren emular. <http://stackoverflow.com/questions/22541681/fail-to-create-android-virtual-device-no-system-image-installed-for-this-targe> Vayan entonces al Android SDK Manager y fíjense qué packages hay disponibles para instalar según la versión de Android que están ejecutando.

### You can't combine swipe dismissal with ActionBar \#1

Si al correr la aplicación aparece en el LogCat ese siguiente mensaje revisar si el device es Android Wear, porque no es compatible con los ejemplos.

### Se cuelga el Android SDK Content Manager cuando levanta el Eclipse (0%)

<http://stackoverflow.com/questions/13489141/eclipse-hangs-at-the-android-sdk-content-loader>

Si se cuelga el Android SDK Content Manager, cerrar el Eclipse asegurándose de que no quedó ningún proceso levantado. Luego ingresar al %USER PROFILE%, ingresar a la carpeta .android y borrar

-   el directorio cache
-   y el archivo ddms.cfg

Luego iniciar el Eclipse normalmente.

### No veo la librería de Android en mi proyecto

Si no aparece la librería de Android, botón derecho sobre el proyecto

-   en la solapa Android elegir un target (por ejemplo Android 4.4W)
-   y luego agregar la librería Android Classpath Container en el build path (Build path &gt; solapa Libraries &gt; Add... Android Classpath Container)

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

