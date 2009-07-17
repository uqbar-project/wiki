Un problema que aparece *muy* frecuentemente al programar en Haskell es el siguiente.

`Instance of Fractional Int required for definition of ...`

Eso puede ocurrir por ejemplo en el siguiente programa, en la función `division`

> `suma []=0`
> `suma (x:xs)= x + suma xs`
>
> `division xs = suma xs / length xs`

Al intene y cuando lo ejecuto me dice:

y no se si me olvide de hacer algo en la funcion,o de declarar algo en algun lado... saludos

Es un problema de tipos de datos (bastante frecuente). Lo que pasa es que length devuelve un entero y la división (/) no está definida para los enternos. Entonces tenés que convertir ese entero (Int) a real (Fractional), asi:

`   division (x:xs)= suma (x:xs) / fromInt (length (xs))`

Además deberías corregir el parámetro de length (¿por qué xs?) Deberías poner:

`   division (x:xs)= suma (x:xs) / fromInt (length (x:xs))`

Y de hecho no es necesario poner (x:xs) en todos lados, más simple sería:

`  division l = suma l / fromInt (length l)`
