---
layout: article
title: Sabores de colecciones
---

Sabores de colecciones en Wollok
--------------------------------

En Wollok disponemos de dos sabores básicos de colecciones: las listas y los sets (conjunto matemático). Se diferencian principalmente por las siguientes características:

-   Las listas tienen orden, los sets no, por eso sólo es posible obtener un determinado elemento en base a su pocisión sólo si es una lista con el mensaje get(posicionBaseCero)
-   Los sets no admiten repetidos, las listas sí, por eso si se agrega un elemento repetido a un set no se agregará una nueva referencia al mismo, mientras que en las listas se incluirá una nueva referencia (en otra posición) al mismo objeto.

Literal de listas: \[1,2,3\] Literal de sets: \#{1,2,3}

Para ejemplos de uso ver [Intro a colecciones](intro-a-colecciones.html)

Sabores de colecciones en Smalltalk
-----------------------------------

Smalltalk tiene una clase llamada **Collection** que me permite representar colecciones, pero Collection es la abstracción superior de la jerarquía de colecciones. Al momento de instanciar una colección, tengo que decidirme por un sabor. A continuación intentaremos mostrar los sabores de colecciones, tratando de ordenarlos en base a sus respectivos grados de especialización.

### Set y Bag

El primer sabor que visitaremos es el **Bag**. Esta colección es de las denominadas **sin orden** y como su nombre lo indica, es la representación de una bolsa. Una bolsa de objetos. Por ejemplo, al principio, hablamos de un carrito de compras. El Bag la colección más adecuada para esta representación. Imagínense que puedo poner tres latas de tomates, dos botellas de agua y siete peras.

El segundo sabor que veremos es el **Set**. El Set, como su nombre lo indica, está concebido para la representación de conjuntos.

Una propiedad, fundamental diría, sobre conjuntos es que los elementos que pertenecen al conjunto son únicos en él. En resumidas palabras, en los conjuntos (Sets) no voy a tener dos veces el mismo elemento. O sea el Set no admite repetidos. Salvo por esta propiedad, el comportamiento es el mismo que el del Bag.

Imagínense que tengo una lata de tomates y miCarrito es una colección. Lo voy a representar así:

`unaLata := LataDeTomates new. `
`unaLata conTomates: ‘perita’. `
`unaLata deLaMarca: ‘La Marca que a uds les guste’ `
`unaLata conPrecio: 1.00 `

Bien, tengo mi objeto unaLata y quiero agregar a mi carrito de compras 3 latas de tomates. Entonces hago:

`miCarrito add: unaLata. `
`miCarrito add: unaLata. `
`miCarrito add: unaLata. `

¿Qué pasará cuando evalúe la siguiente línea?

`miCarrito size.`

Yo agregué 3 veces al carrito una lata de tomates, con lo cual podría esperar que me responda 3. Pero, como vimos, si represento a mi carrito de compras con un Bag, (o sea que hice miCarrito:= Bag new.) entonces sí voy a tener 3 referencias a unaLata en miCarrito.

Si representara mi carrito de compras con un Set (o sea que hice miCarrito:=Set new.) entonces hubiese tenido un solo objeto dentro de mi colección. Simplemente, cuando hago el segundo miCarrito add: unaLata el Set identifica que ya tiene ese objeto en la colección y no lo vuelve a agregar.

### Colecciones ordenadas

Entonces ya vimos el Set y Bag, que son colecciones sin orden. Ahora introduciremos algunas colecciones con orden.

¿Qué quiere decir **ordenadas**? Que hay un elemento 1, un elemento 2, etc., al contrario de un Set o un Bag, en donde los elementos están "todos tirados". O sea, puedo acceder a los elementos de cualquier colección ordenada (veremos que hay varias variantes) parecido a como se accede a un Array en C o Pascal. Para eso le envío a la colección el mensaje **at:** , si quiero el cuarto elemento de miCol que es una colección ordenada, lo puedo pedir así:

`miCol at: 4 `

y tengo los mensajes first y last que devuelven el primero y el último.

Eeeehhh pero … si tengo un Set o un Bag, ¿ cómo accedo a los elementos de un Bag o un Set? Eso ... viene más adelante. Veamos ahora las variantes de colecciones ordenadas.

La primera que les presentaremos es la **OrderedCollection**. Esta colección ordena sus elementos, y el criterio es el orden en cual fueron agregados a la colección. Entonces si creo mi colección así

`miCol := OrderedCollection new. `
`miCol add: 'hola'. `
`miCol add: 'queridos'. `
`miCol add: 'amigos'. `
`miCol add: 'escandinavos'. `

después puedo pedirle varias cosas

`miCol first <- el primero - 'hola'`
`miCol last <- el último - 'escandinavos'`
`miCol at: 3 <- el tercero - 'amigos'`
`miCol at: 4 <- el cuarto - 'escandinavos'`
`miCol size <- cantidad de elementos - 4`

y después puedo seguir agregándole elementos, ante lo cual la colección "se estira"

`miCol add: 'agrego'. `
`miCol add: 'cosas'. `
`miCol size <- cantidad de elementos ahora - 6`
`miCol last <- el último ahora - 'cosas'`

Luego, así como la OrderedCollection, también tenemos la llamada **SortedCollection**, que la diferencia con la primera radica en que el criterio de ordenamiento puede ser definido. Si no definimos el criterio, SortedCollection ordena los elementos por su “orden natural” (significa que los ordenará de menor a mayor). Dicho de otra forma, no ordena por orden de llegada, sino por comparación entre los elementos.

Si queremos ordenar los elementos de la colección con un criterio en particular, necesitamos pasárselo a la colección. La forma de hacerlo, es pasarle lo que denominamos **sortBlock**, que es un objeto [Block](bloques.html) (bloque).

Con respecto a las colecciones que tienen orden, por último veremos al viejo amigo **Array**. Aquí en Smalltalk también existe, y una de sus características es que es de tamaño fijo. Para instanciar un Array, hago Array new: 6, donde 6 es la cantidad de elementos que contendrá, alternativamete si conocemos los elementos de antemano pueden crearse de [ forma literal](arrays-literales-en-smalltalk.html) (que es uno de los pocos motivo razonable para querer usar un Array en vez de otro tipo de colección como OrderedCollection).

Los Arrays no implementan el mensaje add:, justamente porque no puedo modificar su tamaño. La forma de agregarles elementos es a través del mensaje **at:put:**, como por ejemplo:

`miVector := Array new: 2. `
`miVector at: 1 put: unaLata. `

Todas las colecciones entienden una serie de mensajes que permiten obtener distintos sabores de colecciones con los mismos elementos. Estos mensajes son de la forma “as{ColeccionQueQuiero}”. Vamos a un par de ejemplos para ver cómo funciona.

Si tuviese una colección de la clase Bag, y quiero sacarle los repetidos, sé que el Set no tiene repetidos, entonces tomo mi colección como un Set. Entonces:

`sinRepetidos := miCarrito asSet. `

Si tuviese un array, y lo quiero convertir en una colección de tamaño variable, podría hacer:

`coleccionVariable := miVector asOrderedCollection. `

Si quisiera ordenar mi carrito de compras del producto más caro al más barato, haría algo como:

`ordenadosPorPrecio := miCarrito asSortedCollection: [:unProd :otroProd | unProd precio > otroProd precio]. `

El mensaje **asSortedCollection:** recibe un parámetro que, obviamente, es un sortBlock. El sortBlock es un bloque que necesita 2 parámetros. El código del bloque es un código que debe devolver true o false. Para ordenar los objetos dentro de la colección, se evalúa el código y si el objeto retornado es true, el primer parámetro va antes que el segundo. Si retorna false, el segundo parámetro se ubica antes que el primero.

También está el mensaje **asSortedCollection** (o sea sin el dos-puntos, o sea que no requiere parámetro) que, como dijimos antes, ordenará los elementos por el “orden natural”.

¿Cuál es el "orden natural"? Dijimos que si a una SortedCollection no le decimos cómo queremos que ordene los elementos, los ordena por el "orden natural". Pero ... ¿qué puede ser este "orden natural"?

Si estoy en el paradigma de objetos ... seguro va a tener que ver con objetos y mensajes. El "orden natural" es el que dicta el mensaje &lt;, que es binario. O sea, en una SortedCollection con "orden natural" el criterio es poner a elem1 antes que elem2 si el resultado de evaluar **elem1 &lt; elem2** es true.

Claro, eso quiere decir que solamente voy a poder tener, en una SortedCollection con "orden natural", objetos que entiendan el mensaje &lt;. Los números, los String, las fechas, todos esos entienden &lt;. Pero p.ej. si quiero poner latas en una SortedCollection, no puede ser por "orden natural", tengo que especificar el orden con el bloque con dos parámetros como vimos hace un ratito.

### Jerarquía de Colecciones (En Pharo)

Aquí se provee una imagen con una jerarquía simplificada de las colecciones existentes en Pharo. Presten especial atención a las clases explicadas en este artículo, las demás se proveen por motivos de completitud.

<img src="CollectionHierarchy.png" title="Jerarquía de colecciones en Pharo Smalltalk" alt="Jerarquía de colecciones en Pharo Smalltalk" width="600" />

Diccionarios
------------

Por último, queremos mostrarles un sabor de colección que es especial. Se llama Dictionary y, como su nombre lo indica, intenta representar un diccionario. Este tipo de representación implica tener una asociación entre una clave y un valor.

Poniendo como ejemplo, el propio diccionario. El diccionario es una asociación entre una clave, que son cada letra del abecedario y un apartado de páginas que tienen palabras. El diccionario lleva todo una explicación aparte, pueden profundizar más al respecto leyendo sobre [Diccionarios](diccionarios.html).
