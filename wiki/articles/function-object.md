En el paradigma de objetos los mensajes no son objetos, y por tanto no tenemos forma de hablar contra los mensajes, mas allá de las capacidades reflexivas de algunos lenguajes. Dicho de otra forma los mensajes no son valores, no son "cosas".

Esto nos restringe en que no tenemos mucho control sobre estos envios: por ejemplo, no tenemos forma nativa de enviar mensajes parciales o de pasar el envío de un mensaje por parámetro.

Para suplir estas limitaciones, la estrategia usada en objetos consiste justamente en cosificar el envío de mensajes, construyendo objetos que lo modelen. En lenguajes como Smalltalk, tenemos una forma nativa de construir tales objetos: los bloques de código.

Por ejemplo, en Haskell, uno puede escribir los siguiente:

map (1+) \[1,2,3\]

dado que las funciones están currificadas (lo cual se aprecia en la expresión 1+, donde se aplica parcialmente + con el valor 1, devolviendo otra función), y son valores (1+, es un valor que puede ser pasado como argumento a map)

Si el envío de mensajes en Smalltalk fuera un valor, uno bien podría escribir lo siguiente:

1.  (1 2 3) collect: (1+)

(omitiendo el argumento del mensaje +)

o

1.  (1 2 3) collect: (+1)

(omitiendo el receptor del mensaje + 1)
