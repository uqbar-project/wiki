Entendemos por Function Object a un patrón funcional aplicado a objectos que consiste en cosificar el envío de mensajes, para poder resolver problemas empleando orden superior y aplicación parcial de forma similar a como lo haríamos en el paradigma funcional.

El problema
-----------

En el paradigma de objetos los mensajes no son objetos, y por tanto no tenemos forma de hablar contra los mensajes, mas allá de las capacidades reflexivas de algunos lenguajes. Dicho de otra forma los mensajes no son valores, no son "cosas".

Esto nos restringe en que no tenemos mucho control sobre estos envios: por ejemplo, no tenemos forma nativa de enviar mensajes parciales o de pasar el envío de un mensaje por parámetro.

Por ejemplo, en Haskell, uno puede escribir los siguiente:

`map (1+) [1,2,3]`

dado que las funciones están currificadas (lo cual se aprecia en la expresión 1+, donde se aplica parcialmente + con el valor 1, devolviendo otra función), y son valores (1+, es un valor que puede ser pasado como argumento a map)

Si el envío de mensajes en Smalltalk fuera un valor, uno bien podría escribir lo siguiente:

`#(1 2 3) collect: (1+)`

(omitiendo el argumento del mensaje +)

o

`#(1 2 3) collect: (+1)`

(omitiendo el receptor del mensaje + 1)

Para suplir estas limitaciones, la estrategia usada en objetos consiste justamente en cosificar el envío de mensajes, construyendo objetos que lo modelen. En lenguajes como Smalltalk, tenemos una forma nativa de construir tales objetos: los bloques de código.

Por ejemplo, en Smalltalk, la funcionalidad anterior se implementa de la siguiente forma:

`#(1 2 3) collect: [:x | x + 1]`

Lo cual es análogo al siguiente código Haskell:

`map (\x -> x + 1) [1,2,3]`

En el sentido de que explícitamente construye un valor que modela una computación, que los argumentos de la misma son explícitos y están etiquetados. Pero con la diferencia de que en Haskell, el uso de una lambda es redundante, mientras que en Smalltalk es necesario.

Justamente por su semejanza con la definición de una función anónima, a este patrón se lo conoce como Function Object: un objeto que entiende en su versión mas simple un único mensaje (llamado típicamente apply, value, call, eval, etc) y cosifica un envio de mensajes.

Estos objetos pueden entender opcionalmente otros mensajes que permitan implementar la noción de aplicación parcial y/o currificación, y que implementen operaciones típicas sobre funciones como, por ejemplo, la composición.

En lenguajes como Java, dónde no tenemos una sintaxis especifica para instanciar estos bloques de código, tenemos que recurrir a la definición e instanciación explícita de clases, típicamente definiendo una o varias interfaces que representen a las funciones de diferentes aridades, e implementádolas e instanciandolas conjuntamente, mediante el uso de clases anónimas.

Así por ejemplo, es común encontrar encontrar código que emplea bibliotecas (no estándarares) de colecciones de la siguiente forma:

` Collections.map(Arrays.asList(1,2,3), new Function`<Integer, Integer>`() {`
`     public Integer apply(Integer x) { return x + 1; }`
` });`
` `

Lo cual es evidentemente mas verborrágico y tiene mucha mas redundancia que las soluciones anteriores, pero igualmente cumple su cometido.

Function Object y Orden Superior
--------------------------------

Function Object y Closure
-------------------------

Function Object, Strategy y Command
-----------------------------------
