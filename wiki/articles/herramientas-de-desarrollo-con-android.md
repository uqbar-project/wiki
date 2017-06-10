---
layout: article
title: Herramientas de desarrollo con android
featured: true
---

# LogCat

Para poder mostrar información en consola, hay que reemplazar los println por

```java
Log.v() // Verbose`
Log.d() // Debug`
Log.i() // Info`
Log.w() // Warning`
Log.e() // Error`
```

y

```
Log.wtf
```

que muestran los errores por el LogCat, una consola especial de Android.

## Cómo usarlo

```java
Log.w("Librex", libro.toString())
```

El primer parámetro indica la aplicación o agrupador, la segunda es el valor a mostrar

## Cuándo usar cada uno

Leer [esta recomendación](http://stackoverflow.com/questions/7959263/android-log-v-log-d-log-i-log-w-log-e-when-to-use-each-one)


# Notificaciones al usuario

El concepto *toast* permite enviar al frente un mensaje al usuario sin la incomodidad que tiene el popup de tener que confirmar la lectura de ese mensaje. Es una herramienta útil tanto para informar acciones que corrieron en background como para mostrar el estado de la aplicación en modo desarrollo.

Para más información

-   <http://developer.android.com/guide/topics/ui/notifiers/toasts.html>

