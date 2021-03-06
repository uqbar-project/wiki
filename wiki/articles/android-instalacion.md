---
layout: article
title: Preparacion de un entorno de desarrollo android
featured: true
---

<img src="/img/languages/android-logo.png" height="45%" width="45%"/>

# Download e instalación base

La instalación que nosotros recomendamos es la siguiente:

- [**Android Studio**](https://developer.android.com/sdk/index.html)
  - Al descargarlo para tu sistema operativo, se abre un asistente
  - En la presentación, dar Next (Siguiente)
  - Elegir el tipo de instalación Standard (por defecto) y presionar Next (Siguiente)
  - Se visualizan las opciones elegidas, presionamos Next (Siguiente)
  - y un último Finish hace las descargas necesarias (esperar pacientemente)

- **Android SDK**
  - Si vas a trabajar con tu celular como dispositivo de pruebas descargate entonces sa misma versión del SDK (ver más abajo en Configuraciones)
  - Si vas a trabajar con un emulador, descargate la última versión
  - Seguir los pasos que se explican en la [URL de descarga](https://developer.android.com/studio/install.html) (se adapta al sistema operativo e idioma de la máquina)

<!-- -->

# Importante: Configuraciones

Las configuraciones que te recomendamos son:

- Si estás familiarizado con los shortcuts del Eclipse: File &gt; Settings &gt; Keymap y en el combo Keymaps seleccionar Eclipse

<!-- -->

- Para hacer las pruebas tienen dos opciones: utilizar un dispositivo Android conectado a USB (algo que recomendamos si tenés una máquina con menos de 4GB de memoria) o bien configurar un emulador mediante el Android Virtual Device (AVD). El AVD Manager aparece desde el menú Tools &gt; Android &gt; AVD Manager. 

- AVD Name: El nombre que quieran
- Device: Nexus 5 4.95" 1080x1920 420dpi
- Nougat: Android 7.0 x86\_64
- Scale: Auto
- Emulated Performance: Use Host GPU chequeado, Store a snapshot for faster startup deschequeado

Igualmente pueden configurar cualquier otro dispositivo (Phone o Tablet recomendado, no Wear porque tiene características muy diferentes)

> IMPORTANTE: Si usás tu celular como dispositivo de pruebas, tienen que coincidir la versión de tu celular con la que vas a configurar en el proyecto.

<!-- -->

# Cómo bajarse los ejemplos

- Para bajarte los ejemplos o bien usás la opción "Check out project from Version Control" y escribís la dirección de los ejemplos, o bien descargás el repositorio Git como un zip y lo descomprimís aparte, para luego hacer Import Project.

<!-- -->

- Luego reapuntar el SDK al directorio donde se instaló: Tools &gt; Android &gt; SDK Manager: ubican ese directorio Android SDK Location (el que estuvo en el paso $Android\_SDK\_Path). Además deben tener instalada alguna API (API Level xx, Revision x, Status: deben marcarla y luego seleccionar Apply). Una vez instalado reiniciar.

<!-- -->

# Troubleshooting

## "could not find SDK folder"

Implica que no están apuntando al SDK que instalaron o bien que no instalaron el Android SDK. Solución: Tools &gt; Android &gt; SDK Manager, filtran por Android y marcan el SDK Location que corresponda.

## No se puede ejecutar una app: "no devices"

Si al correr ven un mensaje de error que indica que no tienen devices, deben configurar un dispositivo para correr la aplicación Android (Android Virtual Device Manager del menú Window) o bien utilizar un dispositivo real conectado a USB.

## "NO System images installed for this target"

Si aparece [ese error](http://stackoverflow.com/questions/22541681/fail-to-create-android-virtual-device-no-system-image-installed-for-this-targe) cuando quieren configurar un device y no les habilita el botón Ok, esto implica que falta descargar del Android SDK Manager las VM (imágenes) de los dispositivos que quieren emular.  Vayan entonces al Android SDK Manager y fíjense qué packages hay disponibles para instalar según la versión de Android que están ejecutando.

## You can't combine swipe dismissal with ActionBar \#1

Si al correr la aplicación aparece en el LogCat ese siguiente mensaje revisar si el device es Android Wear, porque no es compatible con los ejemplos.

<!-- -->

# Cómo empezar

- <http://developer.android.com/index.html>
- <http://www.vogella.com/android.html>
- Un [tutorial](http://www.youtube.com/watch?v=zS1frzHbKWY) de youtube que va bien tranquilo

<!-- -->

## Herramientas de desarrollo

- [Herramientas de desarrollo con Android](herramientas-de-desarrollo-con-android.html)

<!-- -->

# Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
