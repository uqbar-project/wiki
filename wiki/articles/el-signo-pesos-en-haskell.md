---
layout: article
title: Precedencia de los operadores mas comunes en haskell
---

El signo pesos en Haskell (el `$`) se usa para 2 cosas:

1- La función aplicar
------------------------

`$` es la función "aplicar". Hacer `even $ 4` significa "al even aplicalo pasándole el 4", o sea que es lo mismo que hacer `even 4`.  Se suele usar para componer y hacer algunas cositas chetas como por ejemplo:

Ejemplo, si en la consola quiero usar la función `pam` así:

```hs
Prelude> pam 5 [even, even, odd]
[False, False, True]
```

Es decir, que de una lista de funciones quiero aplicarlas a un valor dado, podría resolverlo así:
```hs
pam :: a -> [(a -> b)] -> [b]
pam cosa funciones = map (\f -> f cosa) funciones

-- otra forma:
-- pam cosa funciones = map (aplicarA cosa) funciones
-- aplicarA cosa funcion = funcion cosa
```

o bien aprovechar la función `$`:

```hs
pam :: a -> [(a -> b)] -> [b]
pam cosa funciones = map ($ cosa) funciones
```

Ahí aprovechamos la aplicación parcial de `$` (la aplicación parcial de la función aplicación 😆) para pasarle la `cosa` que necesita y luego que le falte recibir la función. Luego el map hace la magia de irle pasando todas las funciones de la lista.

2- Evitando paréntesis
------------------------

La función`$` tiene **muy poca precedencia**. Eso significa que a veces la gente la usa para **evitar paréntesis**.

Por ejemplo, esto está mal:
```hs
even 2 * 3
```
Porque _even tiene más precedencia que el `*`_ , eso significa que va a hacer `even 2` y luego multiplicarlo por 3, lo cual es error de tipos.

Entonces, hay que poner paréntesis:
```hs
even (2 * 3)
```
O bien usar el `$` :
```hs
even $ 2 * 3
```
Aprovechando que eso significa "al `even` aplicalo pasándole el `2*3`", y que como tiene muy poca precedencia, hace que todo lo que está a la izquierda se considere una cosa (el `even`), que todo lo que está a la derecha (el `2 * 3`) se considere otra y entonces a `even` le aplica el resultado de `2*3`.

Es un clásico problema de sintaxis, de parsers. Cada lenguaje tiene sus reglas de precedencia para desambiguarlo, [Haskell tiene las suyas](precedencia-de-los-operadores-mas-comunes-en-haskell.html), y se usa el $ como chiche a veces para evitar paréntesis.
