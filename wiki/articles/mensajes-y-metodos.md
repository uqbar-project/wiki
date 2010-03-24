Definiciones
------------

Un **mensaje** es algo que yo le puedo decir a un objeto.

Un **método** es una secuencia de líneas de código que tiene un nombre.

Cuando se le envía un mensaje a un objeto, se activa un método cuyo nombre coincide con el mensaje enviado. La palabra *método* puede entenderse como "forma", describe la forma en que algunos objetos responden a un determinado mensaje cuando alguien se los envía.

P.ej. si tengo un objeto referenciado por la variable pepe, y pongo

` pepe direccion`

entonces

-   estoy enviando el mensaje *direccion*
-   se va a activar un método llamado *direccion*. ¿Qué método? El que decida el [method lookup](paradigma-de-objetos---method-lookup.html)

Si trabajamos con prototipos (por ejemplo con el Object Browser; o un lenguaje como Self o Javascript), entonces los métodos están los objetos.

Si trabajamos con clases (como todo el cuatrimestre excepto al principio), entonces los métodos están en las clases.

Importante no olvidar
---------------------

-   Los mensajes los entienden los objetos
-   Si a un objeto que entiende el mensaje **x** le envio el mensaje **x** entonces se va a activar el método **x** para ese objeto
-   Cuando en un método dice `self`, es una referencia al objeto que recibió el mensaje por el cual se activó el método.
    En el ejemplo, si en el método *direccion* dice `self`, entonces al hacer `pepe` `direccion` este `self` va a ser una referencia al objeto referenciado por `pepe`.

