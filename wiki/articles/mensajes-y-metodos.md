---
layout: article
title: Mensajes y metodos
---

Definiciones
------------

Un **mensaje** es algo que yo le puedo decir a un objeto.

Un **método** es una secuencia de líneas de código que tiene un nombre.

Cuando se le envía un mensaje a un objeto, se activa un método cuyo nombre coincide con el mensaje enviado. La palabra *método* puede entenderse como "forma", describe la forma en que algunos objetos responden a un determinado mensaje cuando alguien se los envía.

P.ej. si tengo un objeto referenciado por la variable pepe, y pongo

` `**`Smalltalk:`**
` pepe direccion`

` `**`Wollok:`**
` pepe.direccion()`

entonces

-   estoy enviando el mensaje *direccion*
-   se va a activar un método llamado *direccion*. ¿Qué método? El que decida el [method lookup](paradigma-de-objetos---method-lookup.html)

¿A quiénes se le pueden mandar mensajes en un método?
-----------------------------------------------------

-   A los objetos que conocidos mediante atributos
-   A los objetos que me pasan por parámetro
-   A mí mismo usando [self](self---pseudovariable.html)
-   A los objetos bien conocidos (incluyendo a los literales)

Importante no olvidar
---------------------

-   Los mensajes los entienden los objetos
-   Si a un objeto que entiende el mensaje **m** le envio el mensaje **m** entonces se va a activar el método **m** para ese objeto
-   Cuando en un método dice [`self`](self---pseudovariable.html), es una referencia al objeto que recibió el mensaje por el cual se activó el método.
    En el ejemplo, si en el método *direccion* dice `self`, entonces al hacer `pepe` `direccion` este `self` va a ser una referencia al objeto referenciado por `pepe`.

