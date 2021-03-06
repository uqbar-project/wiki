---
layout: article
title: No hay instancias para el Show
---

A muchos probablemente les haya sucedido encontrarse con el error `No instance for Show`. 
Esto, contrario a lo que muchos creen, no significa que nos quedamos sin show por falta de instancias.
Lo que significa es, en criollo, _"ARRRGH, no sé mostrar esto en la consola"_

Sólo que lo dice aproximadamente así:

```
<interactive>:2:1:
    No instance for (Show (a0 -> Bool))
      (maybe you haven't applied enough arguments to a function?)
      arising from a use of ‘print’
    In the first argument of ‘print’, namely ‘it’
    In a stmt of an interactive GHCi command: print it
```

Solución rápida
---------------

La mayoría de los datos son “mostrables”. Si yo quiero mostrar un “3”, la consola lo sabe mostrar:

```Haskell
Main> 3
3
```

Pero no puedo mostrar una función:

```Haskell
Main> even
<interactive>:2:1:
   No instance for (Show (a0 -> Bool))
     (...)
```

#### Solución rápida 1
Arriba de todo en el archivo .hs escribir:

```Haskell
import Text.Show.Functions
```

Esto hace que las funciones sean mostrables:

```Haskell
Main> even
<function>
```

#### Solución rápida 2

En realidad, técnicamente lo que estamos haciendo al hacer el import de Text.Show.Functions es lo mismo que agregar esto al principio de nuestro archivo:

```Haskell
instance Show (a -> b) where
  show f = "<una función>"
```

Esto hace que las funciones sean mostrables:

```Haskell
Main> even
<una función>
```

Explicación
-----------

Para entender más sobre el tema, se recomienda la lectura de [Definiendo nuestros Tipos](data--definiendo-nuestros-tipos-en-haskell.html)

También se recomienda la lectura de Learn you a Haskell, el capítulo sobre [clases de tipos](http://aprendehaskell.es/content/Tipos.html#clases-de-tipos-paso-a-paso-1a-parte)

