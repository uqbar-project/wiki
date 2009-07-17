Un problema que aparece *muy* frecuentemente al programar en `Haskell` es el siguiente.

`Instance of Fractional Int required for definition of ...`

Eso puede ocurrir por ejemplo en el siguiente programa, en la función `division`<ref> Si la versión anterior de la función te resulta complicada, tal vez puedas entender mejor esta:

`suma []=0`
`suma (x:xs)= x + suma xs`
`division xs = suma xs / length xs`

</ref>

`suma = foldl (+) 0 `
`division xs = suma xs / length xs`

El problema surge porque el `Haskell` tiene diferentes tipos de valores numéricos (enteros, reales, etc) un sistema de tipos muy estricto que hace que no sea sencillo mezclar los distintos tipos de valores en una misma operación.

La versión corta es que `length` `xs` es un entero y la operación `/` no está definida para los números enteros. Por lo tanto es necesario convertir el resultado de `length` `xs` al tipo de datos adecuado, utilizando la función `fromIntegral`:

Es un problema de tipos de datos (bastante frecuente). Lo que pasa es que length devuelve un entero y la división (/) no está definida para los enternos. Entonces tenés que convertir ese entero (Int) a real (Fractional), asi:

`   division (x:xs)= suma (x:xs) / fromInt (length (xs))`

Además deberías corregir el parámetro de length (¿por qué xs?) Deberías poner:

`   division (x:xs)= suma (x:xs) / fromInt (length (x:xs))`

Y de hecho no es necesario poner (x:xs) en todos lados, más simple sería:

`  division l = suma l / fromInt (length l)`
