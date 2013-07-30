Las colecciones nos resultan de utilidad porque nos permiten agrupar objetos, para luego poder operar sobre un elemento en particular, sobre algunos elementos seleccionados mediante un filtro, o sobre una colección como conjunto.

Esto nos permite modelar conjuntos o agregados de cosas, que son muy comunes en casi todos los dominios en los que podemos pensar: las piezas de un tablero de ajedrez, los integrantes de un equipo de fútbol, las líneas de una factura, ¡de todo!

A primera vista una colección es un conjunto de objetos. Si la vemos con más precisión nos damos cuenta que es más preciso pensarla como un conjunto de referencias: los elementos no están adentro de la colección, sino que la colección los conoce. A su vez, como todo en Smalltalk es un objeto, podemos deducir que una colección también es un objeto.

También sabemos que en Smalltalk todo objeto es instancia de una clase, entonces va a haber una clase Collection. En realidad hay toda una jerarquía con [distintos “sabores”](sabores-de-colecciones.html), distintos tipos de colecciones que nos van a servir para distintos fines.

¿Qué podemos hacer con una colección?
-------------------------------------

Para tratar de responder esta pregunta no es necesario estar familiarizado con las colecciones de Smalltalk. Recordemos que una de las características ventajosas del Paradigma de Objetos es que acerca nuestro modelado a la forma en que percibimos la realidad.

Entonces ¿que haríamos nosotros con una colección? Pensemos en una colección de estampillas. Podemos:

-   Mirarlas -&gt; recorrer la colección
-   Encuadernarlas por fecha -&gt; ordenar la colección
-   Conseguir nuevas estampillas -&gt; agregar elementos a la colección
-   Regalar una estampilla -&gt; quitar elementos de la colección.
-   Quedarme con las estampillas de Brasil -&gt; hacer un filtro o selección de los elementos según un criterio.
-   Saber si tengo una determinada estampilla -&gt; preguntarle a una colección si incluye o no un determinado objeto como elemento.

¿Qué otra colección podemos modelar del mundo real? ¡¡Un carrito del supermercado!!

Mientras vamos de góndola a góndola del super vamos agregándole elementos (referencias a objetos) a esa colección. Cuando llegamos a la caja recorremos esa colección y obtenemos información de la misma: cuánto suma el costo total de sus artículos, cuántos artículos compré...

¿Ahora, como podemos modelar todas estas situaciones con objetos usando Smalltalk?... ¡Enviando mensajes a colecciones! Recordemos que en el paradigma de objetos todo programa es un conjunto de objetos enviándose mensajes para concretar un objetivo en común. Bien, en este caso los objetos serán las distintas colecciones y sus elementos. Los mensajes que puede recibir una colección serán, entre otros, los que mencionamos para la colección de estampillas, dándoles un toque smalltalkero ;-)

Un ejemplo rápido: La primer clase que vamos a usar y que implementa la idea de colección se llama Set. Entonces, para crear una colección y asignarla a una variable, escribiremos

`coleccionEstampillas := Set new. `

Ahora definamos tres nuevas estampillas y agreguémoslas a la colección, enviándole el mensaje add:. Defino una cuarta estampilla que no agrego.

`pilla := Estampilla new. `
`pilla origen: 'Brasil'. `
`pilla alto: 6 ancho: 3. `
`pilla2 := Estampilla new. `
`pilla2 origen: 'Alemania'. `
`pilla2 alto: 4 ancho: 2. `
`pilla3 := Estampilla new. `
`pilla3 origen: 'Brasil'. `
`pilla3 alto: 5 ancho: 3. `
`pilla4 := Estampilla new. `
`pilla4 origen: 'Brasil'. `
`pilla4 alto: 3 ancho: 1. `
`coleccionEstampillas add: pilla. `
`coleccionEstampillas add: pilla2. `
`coleccionEstampillas add: pilla3. `

A esta colección ya le puedo hacer algunas preguntas

`coleccionEstampillas size. "devuelve 3"`
`coleccionEstampillas includes: pilla2. "devuelve true"`
`coleccionEstampillas includes: pilla4. "devuelve false"`

Claro que para que la colección me sea realmente útil, me debe permitir interactuar con sus elementos, poder hablarle (p.ej.) a pilla2 a través de la colección. Antes de ver cómo hacer esto, clarifiquemos un poco la relación entre una colección y sus elementos.

Colecciones y referencias
-------------------------

Como ya dijimos en la introducción, si miramos una colección en detalle, vemos que es mejor entenderla como un conjunto de referencias a objetos, y no como un conjunto de objetos. ¿Cuál es la diferencia? Que conceptualmente, la colección no tiene “adentro suyo” a sus elementos.

Los elementos de una colección son objetos como cualesquiera otros, al agregarlos a una colección estoy, en realidad, agregando una referencia que parte de la colección y llega al objeto “agregado”. Los objetos que elijo agregar una colección pueden estar referenciados por cualesquiera otros objetos.

En el ejemplo anterior, algunas de las estampillas que creamos son elementos de la colección, y además están referenciadas por variables. Gráficamente tenemos: ![](Pdep-colecciones-1.PNG "fig:Pdep-colecciones-1.PNG")

Tres aclaraciones sobre el gráfico:

1. por una cuestión de espacio detallamos el estado interno sólo para una estampilla, está claro que todas las estampillas conocen a un String que representa su origen y a un Point que representa su tamaño.

2. la clase Point sí existe en Dolphin (y en todos, o la gran mayoría, de los ambiente Smalltalk) y sirve para representar p.ej. puntos en coordenadas de dos dimensiones o superficies rectangulares.

3. ¿en qué orden “están” las estampillas en el Set? Un Set no mantiene sus elementos en un orden determinado, más adelante veremos que hay distintos “sabores” de colecciones, algunos mantienen orden y otros no.

Ahora agreguemos un objeto más

`pedro := Coleccionista new. `
`pedro estampillaPreferida: pilla3. `

El ambiente queda así:

![](Pdep-colecciones-2.PNG "fig:Pdep-colecciones-2.PNG") (omitimos el nombre de la variable del coleccionista también por cuestiones de espacio)

Ahora hay un objeto que tiene 3 referencias:

1. es el objeto referenciado por la variable pilla3

2. es un elemento del Set

3. es la estampilla preferida del Coleccionista

Ya podemos ver un poco más en detalle la relación entre una colección y sus elementos. La colección maneja referencias a los elementos que le voy agregando (p.ej. enviándole el mensaje add: ), análogas a las referencias de las variables de instancia de otros objetos. Hay dos diferencias entre las referencias que mantiene un Set y las que mantiene p.ej. una Estampilla:

-   Cada referencia de la Estampilla tiene un nombre, que es el nombre de la variable de instancia; las del Set son anónimas.
-   Cada estampilla tiene una cantidad fija de referencias (son siempre 2), un Set puede tener una cantidad arbitraria, que crece a medida que le agrego elementos al Set.

Así, los objetos que quedan referenciados por la colección pueden tener otras referencias sin problema. Un objeto no tiene nada especial por ser elemento de una colección, sólo una referencia más hacia él. Un objeto no conoce, en principio, de qué colecciones es elemento (podría tener una referencia explícita a la colección, pero eso habría que programarlo a mano).

La referencia a un objeto por ser elemento de una colección cuenta para que el objeto no salga del ambiente cuando pasa el Garbage Collector. Veámoslo con un ejemplo, agregando esta línea de código:

`pilla2 := 4 `

El ambiente queda así:

![](Pdep-colecciones-3.PNG "Pdep-colecciones-3.PNG")

La estampilla que marcamos en verde ya no está referenciada por la variable pilla2, pero sigue viva en el ambiente, porque tiene la referencia del Set.

Hablando con los elementos
--------------------------

Hay algunas operaciones que se hacen sobre una colección, en la que parte de lo que hay que hacer, es responsabilidad de cada elemento.

Por ejemplo, supongamos que quiero obtener, de mi colección de estampillas, aquellas que tengan más de 10 cm2 de superficie. La colección no sabe la superficie de cada estampilla, sí conoce a las estampillas, entonces puede enviarle mensajes a cada una. Lo que no sabe es qué mensajes puede enviarle, un Set no sabe si lo que tiene son estampillas, perros, números, otros Set, o cualquier otro objeto, sólo representa al conjunto, sin saber nada de sus elementos. Por otro lado, cada estampilla no sabe en qué colección está, de hecho un mismo objeto podría estar en varias colecciones.

Por lo tanto, para resolver mi problema necesito que actúen tanto la colección (que es la que conoce a los elementos) como cada elemento (que es el que sabe su superficie). Veamos cómo lograr esta interacción. Empecemos por decidir a quién le pedimos lo que queremos. Quiero aquellas estampillas, de las que son elementos de coleccionEstampillas, que cumplan una determinada condición.

¿A qué objeto le voy a pedir esto? A la colección.

El selector (nombre del mensaje) es select: ...

... o sea que necesita un parámetro. Este parámetro va a representar la condición, que es un código que se va a evaluar sobre cada elemento, y debe devolver true o false.

Los objetos que representan "cachos de código" son los [Bloques](bloques.html), en este caso un bloque con un parámetro. Queda así:

`coleccionEstampillas select: [:estam | estam superficie > 10] `

Veamos "cómo es que funciona": El bloque es el que sabe qué preguntarle a cada estampilla, representa la condición. Cambiemos el código anterior un poco

`condicion := [:estam | estam superficie > 10]. `
`coleccionEstampillas select: condicion. `

Ahora el bloque (que es un objeto, tan objeto como el que representa una estampilla, o el que representa un conjunto) está referenciado por una variable. Entonces puedo evaluar, por ejemplo, la condición para pilla. Los bloques entienden el mensaje value:. Lo escribo así:

`condicion value: pilla `

si evalúo esto, devuelve true.

La coleccionEstampillas es la que conoce a sus elementos. Sabe que cuando le llega el mensaje select: con el bloque de parámetro, lo que tiene que hacer es evaluar ese bloque con cada uno de sus elementos (los elementos de coleccionEstampillas). Esta es la parte que maneja la colección.

Las estampillas entienden el mensaje superficie, eso es lo que sabe hacer cada estampilla, devolver su superficie.

Finalmente, ¿qué devuelve esto?

Otra colección, distinta de coleccionEstampillas, y que tiene como elementos algunos de los objetos que también tiene coleccionEstampillas como elementos; exactamente los que cumplen la condición que le paso como parámetro. Si pongo

`grandes := coleccionEstampillas select: [:estam | estam superficie > 10] `

el ambiente va a quedar así:

![](Pdep-colecciones-4.PNG "Pdep-colecciones-4.PNG")

En este diagrama, podemos ver...

1. que el resultado del select: es una colección distinta de coleccionEstampillas

2. que coleccionEstampillas no se modificó como resultado del select:

3. que hay dos estampillas que son elementos de dos colecciones.

Ahora bien, si agregamos a coleccionEstampillas una nueva estampilla grande, ¿se agrega también en la colección referenciada por grandes?

No, porque grandes se creó en el select: y está separada de coleccionEstampillas.

En resumen: cuando quiero hacer una operación sobre una colección que necesita enviarle mensajes a cada elemento, la operación se la pido a la colección, y le voy a enviar como parámetro un bloque que describe la interacción con cada elemento.
