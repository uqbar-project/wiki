---
layout: article
title: Errores comunes  usar un predicado como si fuera una variable
---

A veces se utilizan predicados unarios para representar *verdades globales* (lo que en otro paradigma sería una constante global).

Por ejemplo podemos tener un predicado que nos diga la fecha de hoy:

`hoy(fecha(2009, 10, 15)).`

O podemos tener un predicado que refleje el estado del tablero en un dominó:

`estado([ficha(0,1),ficha(1,4),ficha(4,2),ficha(2,2),ficha(2,5)]).`

El error consiste en utilizar `hoy` o `estado` como si *fueran* constantes, y no lo son.

#### Ejemplo 1 - Fecha de hoy

Suponiendo que tengo en la base de conocimientos información sobre los downloads que se hicieron el día de hoy, en el predicado que relaciona un usuario, con un archivo bajado y una fecha; una forma incorrecta de utilizarlo sería:

`bajoAlgunArchivoHoy(Usuario):-`
`  download(Usuario, _Archivo, Fecha), Fecha = hoy. %INCORRECTO!`

La forma correcta de hacerlo podría ser:

`bajoAlgunArchivoHoy(Usuario):-`
`  download(Usuario, _Archivo, Fecha), hoy(Fecha). `

Se ve que en la nueva versión, hoy se utiliza como predicado y no como individuo.

#### Ejemplo 2 - Estado del dominó

Si quiero saber los extremos de la lista que representa el estado del juego, es incorrecto hacer cosas como:

`extremo(E):-estado = [E|_].         %INCORRECTO!!`
`extremo(E):-ultimoLista(E, estado). %INCORRECTO!!`

La forma correcta de hacerlo sería:

`extremo(E):-estado([E|_]).`
`extremo(E):-estado(Estado), ultimoLista(E, Estado).`

Asumiendo que el predicado relaciona a una lista con su último elemento.
