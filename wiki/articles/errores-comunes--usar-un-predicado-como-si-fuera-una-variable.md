A veces se utilizan predicados unarios para representar *verdades globales* (lo que en otro paradigma sería una constante global).

Por ejemplo podemos tener un predicado que nos diga la fecha de hoy:

`hoy(fecha(2009, 10, 15)).`

O podemos tener un predicado que refleje el estado del tablero en un dominó:

`estado([ficha(0,1),ficha(1,4),ficha(4,2),ficha(2,2),ficha(2,5)]).`

El error consiste en utilizar `hoy` o `estado` como si *fueran* constantes, y no lo son.

Ejemplo usando fechas:  

`bajoAlgunArchivoHoy(Usuario):-`
`  download(Usuario, _Archivo, Fecha), Fecha = hoy. %INCORRECTO!`

La forma correcta de hacerlo sería:

`bajoAlgunArchivoHoy(Usuario):-`
`  download(Usuario, _Archivo, Fecha), hoy(Fecha). `

Se ve que en la nueva versión, hoy se utiliza como predicado y no como individuo.
