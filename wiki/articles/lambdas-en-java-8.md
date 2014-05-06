En la versión 8 del lenguaje [Java](java.html) se introduce una herramienta fundamental para cualquier lenguaje de programación orientado a objetos y que era una de sus principales falencias: las [expresiones lambda](expresiones-lambda.html), también conocidas como [bloques](bloques.html) de código, closures, o funciones anónimas.

Con ellas podemos implementar fácilmente mensajes de alto nivel de abstracción, que reciben o devuelven bloques de código y se comportan de forma parecida a las funciones de [orden superior](orden-superior.html) que podemos encontrar en el [Paradigma Funcional](paradigma-funcional.html).

Dado que el lenguaje Java existió mucho tiempo sin presentar Lambdas (a partir de utilizar [Clases Anónimas](clases-anonimas-en-java.html)), estas presentan características distintivas si las comparamos con los bloques de Ruby o Smalltalk, o las funciones anónimas de Scala o C\#.

Uso básico
----------

En una primera aproximación, una lambda es cualquier objeto que implementa algunas de las siguientes interfaces:

-   Function: una función que toma un sólo argumento
-   Predicate: una función que toma un sólo argumento pero que devuelve exclusivamente booleanos
-   Consumer: una función que toma un sólo argumento y no devuelve nada, probablemente porque produce un [efecto](efecto.html). Es decir, los Consumers normalmente NO son computaciones puras.
-   Entre otras, ver: <http://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html>

Para, por ejemplo, recibirlas por parámetro, debemos simplemente tipar al parámetro de nuestro método con alguna de estas interfaces.

Ejemplo:

`Persona primerPersonasQueCumple(Predicate`<Persona>` predicado) {`
`  for(Persona persona : personas) `
`    if (predicado.test(persona))`
`      return persona;`
` throw new PersonaNoExisteException();`
`}`

Para pasarlas por parámetro, la sintaxis es la siguiente:

`(TipoParametro parametro) -> cuerpo`

que es análogo al siguiente bloque en Smalltalk (recordar que en Smalltalk las variables no se tipan explícitamente):

` [ :parametro | cuerpo  ]`

Ejemplo:

`primerPersonaQueCumple((Persona p) -> p.esMayorDeEdad())`

En muchos casos como el anterior tipo del parámetro puede ser obviado, cuando este puede ser inferido por el contexto:

`primerPersonaQueCumple(p -> p.esMayorDeEdad())`

Referencias a métodos
---------------------

Si el cuerpo del método es el envío de un sólo mensaje (como en el ejemplo anterior), entonces podemos usar una MethodReference:

`primerPersonaQueCumple(Persona::esMayorDeEdad)`

Interfaces de un sólo mensaje
-----------------------------

Por motivos de retrocompatibilidad, en realidad, cualquier Interface que defina un sólo mensaje puede ser usada con la sintaxis de lambda. Ejemplo:

`interface ChequeadorDePersona {`
`   boolean chequear(Persona p);`
`}`

`Persona primerPersonasQueCumple(ChequeadorDePersona predicado) {`
`  for(Persona persona : personas) `
`    if (predicado.chequear(persona))`
`      return persona;`
` throw new PersonaNoExisteException();`
`}`

Y se usa exactamente igual. De todas formas, si no pensamos darle alguna semántica particular a nuestro bloque de código, preferiremos usar normalente las interfaces estándar comentadas antes

Colecciones en Java
-------------------

Una de las principales utilidades de las lambdas es el manejo de colecciones. Java 8 incorpora mensajes a sus colecciones para poder transformarlas usando mensajes análogos a las funciones de orden superior de Haskell: map, filter y reduce(fold), limit(take), entre otros: <http://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html>

La forma de trabajar siempre es la misma: cuando tengamos una colecciones, le enviaremos el mensaje stream() para obtener una secuencia potencialmente infinita (análoga a las listas de Haskell), al cual le podemos enviar mensajes para filtrar, mapear, etc. Cuando hayamos terminado, y si queremos reconvertir nuestro Stream a una colección (como una List, Set, o Collection), le enviaremos el mensaje collect, indicandole a que tipo de colección queremos convertirlo:

-   collect(toList()) (análogo al asOrderedCollection de ST)
-   collect(toSet()) (análogo al asSet de ST)
-   entre otros. Ver <http://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html>

Ejemplo:

` List`<Persona>` personas = Arrays.asList(jose, pedro, maria, anabela);`
` Set`<Persona>` nombresDeDocentesSinRepetidos = personas.stream().filter(Persona::esDocente).map(Persona::getNombre).collect(toSet());`

Lo cual es análogo al siguiente código Smalltalk:

` personas := { jose. pedro. maria. anabela }`
` nombresDeDocentesSinRepetidos := ((personas select: [ :p | p esDocente ]) collect: [ :p | p nombre ] ) asSet`

Ordenar
-------

El orden superior se puede aprovechar para ordenar colecciones en Java. Supongan que tienen:

`public class Foo {`
`    private String bar;`
`    private int baz;`
`   // y sus getters y constructor`
` } `

Cuando quieran tener algo ordenado según un criterio, antes o después, necesitarán un Comparator: es un objeto que nos dice si un objeto es "menor" que otro (precede a otro, dirían en discreta). La buena noticia es que normalmente no tendrán que declarar una clase que implemente esta interfaz, sino que podrán definirlo usando una lambda. Por ejemplo, si quieren crear una priorirty queue que esté ordenada según bar, pueden hacer:

`PriorityQueue`<Foo>` foos = new PriorityQueue<>((x, y) -> x.getBar().compareTo(y.getBar()));`

En general ni siquiera es necesario hacer la comparación a mano. Si quieren ordenar por una propiedad (como en este caso) pueden utilizar Comparator.comparing:

` PriorityQueue`<Foo>` foos = new PriorityQueue<>(Comparator.comparing(foo -> foo.getBar()));`

o lo que es lo mismo:

` PriorityQueue`<Foo>` foos = new PriorityQueue<>(Comparator.comparing(Foo::getBar));`

Si tienen que ordenar por multiples propiedades, pueden utilizar el mensaje thenComparing. Ejemplo de creación de un TreeSet que ordene segun bar y luego segun baz:

` Set`<Foo>` foos = new TreeSet<>(Comparator.comparing(Foo::getBar).thenComparing(Foo::getBaz));`

Finalmente, si tienen que ordenar al revés del orden de precedencia, pueden usar el mensaje reversed. Por ejemplo, acá se está obteniendo un stream ordenado por baz, de mayor a menor:

` Arrays.asList(new Foo("hola", 2), new Foo("hello", 9)).stream().sorted(Comparator.comparing(Foo::getBaz).reversed()) ;`

Para más información, miren la documentación de Comparator: <http://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html>

Más información
---------------

Para más información consultar:

-   <http://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html>
-   <http://docs.oracle.com/javase/tutorial/java/javaOO/methodreferences.html>

