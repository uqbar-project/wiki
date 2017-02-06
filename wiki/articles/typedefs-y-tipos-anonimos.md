---
layout: article
title: Typedefs y tipos anonimos
---

# Descripción

*typedef* es una construcción del lenguaje C que nos permite darle alias a tipos existentes (sinónimos de tipos), de forma similar al *type* de Haskell.

Esto es particularmente útil a la hora de trabajar en ANSI C con estructuras. Por ejemplo, si definimos una estructura TanqueDeAgua de la siguiente forma:

```C
struct TanqueDeAgua {
  int capacidad_maxima;
  int temperatura_del_agua;
  //etc
} 
```

cada vez que querramos utilizar este tipo deberemos escribir:

```C
struct TanqueDeAgua un_tanque;
```

Lo cual es ciertamente verborrágico, y nos acopla mucho más a la implementación: cuando hablo de un tanque de agua, no me interesa si es un *struct*, un *union* u otra cosa.

La solución a este problema es usar un sinónimo de tipo:

```C
typedef struct _TanqueDeAgua {
  int capacidad_maxima;
  int temperatura_del_agua;
  //etc
} TanqueDeAgua
```

Es decir, *TanqueDeAgua* es ahora un sinónimo para *struct TanqueDeAgua*, con lo que puedo escribir

TanqueDeAgua un_tanque.

De hecho, dado que siempre referiremos a este tipo a través de su sinónimo, no es necesario darle un nombre a la estructura \_TanqueDeAgua, ya que podemos definir una estructura anónima:

```C
typedef struct {
  int capacidad_maxima;
  int temperatura_del_agua;
  //etc
} TanqueDeAgua
```
