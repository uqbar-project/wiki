---
layout: article
title: Intro a colecciones
---

Las colecciones nos resultan de utilidad porque nos permiten agrupar objetos, para luego poder operar sobre un elemento en particular, sobre algunos elementos seleccionados mediante un filtro, o sobre una colección como conjunto.

Esto nos permite modelar conjuntos o agregados de cosas, que son muy comunes en casi todos los dominios en los que podemos pensar: las piezas de un tablero de ajedrez, los integrantes de un equipo de fútbol, las líneas de una factura, ¡de todo!

A primera vista una colección es un conjunto de objetos. Si la vemos con más precisión nos damos cuenta que es más preciso pensarla como un conjunto de referencias: los elementos no están adentro de la colección, sino que la colección los conoce. Y como no podía ser de otra forma, las colecciones también son objetos.

No hay un único tipo de colección, hay distintos [sabores de colecciones](sabores-de-colecciones.html) que nos van a servir para distintos fines, sin embargo la mayoría de las colecciones entiende un conjunto grande de [ mensajes en común](mensajes-de-colecciones.html), o sea, son polimórficas :) Sólo van a diferir en algunos mensajes particulares debido a la naturaleza de la colección.

¿Qué podemos hacer con una colección?
-------------------------------------

Para tratar de responder esta pregunta no es necesario estar familiarizado con la implementación de las colecciones. Recordemos que una de las características ventajosas del Paradigma de Objetos es que acerca nuestro modelado a la forma en que percibimos la realidad.

Entonces ¿que haríamos nosotros con una colección? Pensemos en una colección de estampillas. Podemos:

-   Mirarlas -&gt; recorrer la colección y hacer algo con cada elemento
-   Encuadernarlas por fecha -&gt; ordenar la colección
-   Conseguir nuevas estampillas -&gt; agregar elementos a la colección
-   Regalar una estampilla -&gt; quitar elementos de la colección.
-   Quedarme con las estampillas de Brasil -&gt; hacer un filtro o selección de los elementos según un criterio.
-   Saber si tengo una determinada estampilla -&gt; preguntarle a una colección si incluye o no un determinado objeto como elemento.

¿Qué otra colección podemos modelar del mundo real? ¡¡Un carrito del supermercado!!

Mientras vamos de góndola a góndola del super vamos agregándole elementos (referencias a objetos) a esa colección. Cuando llegamos a la caja recorremos esa colección y obtenemos información de la misma: cuánto suma el costo total de sus artículos, cuántos artículos compré...

¿Ahora, como podemos modelar todas estas situaciones en objetos?... ¡Enviando mensajes a colecciones! Recordemos que en el paradigma de objetos todo programa es un conjunto de objetos enviándose mensajes para concretar un objetivo en común. Bien, en este caso los objetos serán las distintas colecciones y sus elementos. Los mensajes que puede recibir una colección serán dependientes del lenguaje, claramente, pero la idea detrás es básicamente la misma.

Un ejemplo rápido: En este ejemplo vamos a usar un tipo de colección al que llamamos Set. Supongamos que tenemos 4 objetos distintos que son estampillas (nos interesa que nos sepan decir su origen y su superficie, no importa cómo lo hacen), agreguemos algunas a la colección.

**`Smalltalk`**
`coleccionEstampillas := Set new.  (disclaimer, en Ozono crear un nuevo conjunto desde Referencias para usarlo directo en el workspace)`
`coleccionEstampillas add: estampillaBrasileraGrande . `
`coleccionEstampillas add: estampillaAlemana . `
`coleccionEstampillas add: estampillaBrasileraMediana . `

**`Wollok`**
`var coleccionEstampillas = #{}`
`coleccionEstampillas.add(estampillaBrasileraGrande)`
`coleccionEstampillas.add(estampillaAlemana)`
`coleccionEstampillas.add(estampillaBrasileraMediana)`

Alternativamente, si ya conozco los elementos que va a tener inicialmente, usando el set literal de Wollok podemos crear directamente la colección con esos elementos.

`var coleccionEstampillas = #{estampillaBrasileraGrande, estampillaAlemana, estampillaBrasileraMediana}`

A esta colección de estampillas ya le puedo hacer algunas preguntas

**`Smalltalk`**
`coleccionEstampillas size. "devuelve 3"`
`coleccionEstampillas includes: estampillaAlemana . "devuelve true"`
`coleccionEstampillas includes: estampillaBrasileraChica . "devuelve false"`

**`Wollok`**
`coleccionEstampillas.size() // devuelve 3`
`coleccionEstampillas.contains(estampillaAlemana) // devuelve true`
`coleccionEstampillas.contains(estampillaBrasileraChica) // devuelve false`

Claro que para que la colección me sea realmente útil, me debe permitir interactuar con sus elementos, poder hablarle (p.ej.) a estampillaAlemana a través de la colección. Antes de ver cómo hacer esto, clarifiquemos un poco la relación entre una colección y sus elementos.

Colecciones y referencias
-------------------------

Como ya dijimos en la introducción, si miramos una colección en detalle, vemos que es mejor entenderla como un conjunto de referencias a objetos, y no como un conjunto de objetos. ¿Cuál es la diferencia? Que conceptualmente, la colección no tiene “adentro suyo” a sus elementos.

Los elementos de una colección son objetos como cualesquiera otros, al agregarlos a una colección estoy, en realidad, agregando una referencia que parte de la colección y llega al objeto “agregado”. Los objetos que elijo agregar una colección pueden estar referenciados por cualesquiera otros objetos.

En el ejemplo anterior, algunas de las estampillas que creamos son elementos de la colección, y además están referenciadas por variables. Gráficamente tenemos: ![](Pdep-colecciones-1.PNG "fig:Pdep-colecciones-1.PNG")

Si se están preguntando ¿en qué orden “están” las estampillas en el Set? Un Set no mantiene sus elementos en un orden determinado, más adelante veremos que hay distintos “sabores” de colecciones, algunos mantienen orden y otros no. En este momento, no es lo que nos interesa.

Ahora agreguemos un objeto más: pedro, el coleccionista, que sabe cuál es su estampilla favorita (la estampillaBrasileraMediana). El ambiente queda así:

![](Pdep-colecciones-2.PNG "Pdep-colecciones-2.PNG")

Ahora hay un objeto que tiene 3 referencias:

1. es el objeto referenciado por la variable estampillaBrasileraMediana

2. es un elemento del Set

3. es la estampilla preferida de pedro

Ya podemos ver un poco más en detalle la relación entre una colección y sus elementos. La colección maneja referencias a los elementos que le voy agregando (p.ej. enviándole el mensaje add: / add(elemento) ), análogas a las referencias de las variables de instancia de otros objetos. Hay dos diferencias entre las referencias que mantiene una colección y las que mantienen nuestros objetos p.ej. pedro:

-   Las referencia que usan nuestros objetos tienen un nombre, que es el nombre que luego usará para hablarle al objeto referenciado; las de la colección son anónimas.
-   Nuestros objetos tienen una cantidad fija de referencias (en este caso pedro tiene una única referencia, estampillaPreferida), una colección puede tener una cantidad arbitraria, que puede crecer a medida que le agrego elementos.

Así, los objetos que quedan referenciados por la colección pueden tener otras referencias sin problema. Un objeto no tiene nada especial por ser elemento de una colección, sólo tiene una referencia más hacia él. Un objeto no conoce de qué colecciones es elemento (podría tener una referencia explícita a la colección para saberlo, pero eso habría que programarlo a mano y por lo general tampoco nos interesa).

La referencia a un objeto por ser elemento de una colección cuenta para que el objeto no salga del ambiente cuando pasa el [Garbage collector](garbage-collector.html). Eso significa que si dejamos de referenciar a nuestra estampilla alemana mediante la referencia estampillaAlemana, como la colección de estampillas la conoce, el objeto va a seguir en el sistema.

![](Pdep-colecciones-3.PNG "Pdep-colecciones-3.PNG")

Hablando con los elementos
--------------------------

Hay algunas operaciones que se hacen sobre una colección en las cuales parte de lo que hay que hacer es responsabilidad de cada elemento.

Por ejemplo, supongamos que quiero obtener de mi colección de estampillas aquellas que tengan más de 10 cm2 de superficie. La colección no sabe la superficie de cada estampilla, sí conoce a las estampillas, entonces puede enviarle mensajes a cada una. Lo que no sabe es qué mensajes puede enviarle, un Set no sabe si lo que tiene son estampillas, perros, números, otros Set, o cualquier otro objeto, sólo representa al conjunto, sin saber nada de sus elementos. Por otro lado, cada estampilla no sabe en qué colección está, de hecho un mismo objeto podría estar en varias colecciones.

Por lo tanto, para resolver mi problema necesito que actúen tanto la colección (que es la que conoce a los elementos) como cada elemento (que es el que sabe su superficie). Veamos cómo lograr esta interacción. Empecemos por decidir a quién le pedimos lo que queremos. Quiero aquellas estampillas, de las que son elementos de coleccionEstampillas, que cumplan una determinada condición.

¿A qué objeto le voy a pedir esto? A la colección.

El selector (nombre del mensaje) es select: en Smalltalk y filter(algo) en Wollok...

... o sea que necesita un parámetro. Este parámetro va a representar una condición para el filtrado, que es un código que la colección debería evaluar sobre cada elemento, y debe devolver true o false para que la colección sepa si debería o no estar en la colección nueva a devolver.

Los objetos que representan "cachos de código" son los [Bloques](bloques.html), en este caso un bloque con un parámetro

**`Smalltalk`**
`coleccionEstampillas select: [:estampilla | estampilla superficie > 10] `

**`Wollok`**
`coleccionEstampillas.filter({estampilla => estampilla.superficie() > 10})`

Mientras que todos los elementos de la colección entiendan el mensaje superficie y al recibirlo retornen un número, el filtrado va a funcionar correctamente.

En resumen: cuando quiero hacer una operación sobre una colección que necesita enviarle mensajes a cada elemento, la operación se la pido a la colección, y le voy a enviar como parámetro un bloque que describe la interacción con cada elemento.
