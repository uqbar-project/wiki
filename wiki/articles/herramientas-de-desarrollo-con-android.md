### LogCat

Para poder mostrar información en consola, hay que reemplazar los println por

`Log.v() // Verbose`
`Log.d() // Debug`
`Log.i() // Info`
`Log.w() // Warning`
`Log.e() // Error`

y

`Log.wtf`

que muestran los errores por el LogCat, una consola especial de Android.

#### Cómo usarlo

`Log.w("Librex", libro.toString()) `

El primer parámetro indica la aplicación o agrupador, la segunda es el valor a mostrar

#### Cuándo usar cada uno

Es una buena recomendación leer <http://stackoverflow.com/questions/7959263/android-log-v-log-d-log-i-log-w-log-e-when-to-use-each-one>

#### No veo la ventana LogCat

Si no la ves en tu perspectiva Java: Window &gt; Show View &gt; Other &gt; escribimos LogCat y seleccionamos la que no está deprecada. También es buena opción subir el nivel de Log para no perdernos en la maraña de mensajes que emite el emulador.

Notificaciones al usuario
-------------------------

El concepto "toast" permite enviar al frente un mensaje al usuario sin la incomodidad que tiene el popup de tener que confirmar la lectura de ese mensaje. Es una herramienta útil tanto para informar acciones que corrieron en background como para mostrar el estado de la aplicación en modo desarrollo.

<http://developer.android.com/guide/topics/ui/notifiers/toasts.html>

### Tips para cuando tenés problemas

**IMPORTANTE:** no definas inner classes, esto causa que el IDE empiece a tirar popups de errores cada vez que refresca su estado y es imposible trabajar.

En caso de tener inestabilidad en tu IDE (algo que puede ocurrir dada la inmadurez de las herramientas de Xtend y su relación con Eclipse) nuestra recomendación es que busques el error para ver si es un issue conocido, y que trates de tener la última versión tanto de Eclipse como de Xtend.

Algunos tips rápidos

-   un Project &gt; Clean, suele ser suficiente.
-   en otros casos puede ser útil cerrar el IDE, renombrar el archivo .metadata y volver a abrir el Eclipse reimportando los proyectos de a uno

Otro material:

-   <http://www.vogella.com/tutorials/AndroidDevelopmentProblems/article.html>

