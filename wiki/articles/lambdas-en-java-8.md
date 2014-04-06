En la versión 8 del lenguaje [Java](java.html) se introduce una herramienta fundamental para cualquier lenguaje de programación orientado a objetos y que era una de sus principales falencias: las lambdas, también conocidas como [bloques](bloques.html) de código, closures, o funciones anónimas.

Con ellas podemos implementar fácilmente mensajes de alto nivel de abstracción, que reciben o devuelven bloques de código y se comportan de forma parecida a las funciones de [orden superior](orden-superior.html) que podemos encontrar en el [Paradigma Funcional](paradigma-funcional.html).

Dado que el lenguaje Java existió mucho tiempo sin presentar Lambdas, estas presentan características distintivas si las comparamos con los bloques de Ruby o Smalltalk, o las funciones anónimas de Scala o C\#.

Uso básico
----------

En una primera aproximación, una lambda es cualquier objeto que implementa algunas de las siguientes interfaces:

-   Function: una función que toma un sólo argumento
-   Predicate: una función que toma un sólo argumento pero que devuelve exclusivamente booleanos
-   Consumer: una función que toma un sólo argumento y no devuelve nada, probablemente porque produce un [efecto](efecto.html). Es decir, los Consumers normalmente NO son computaciones puras.
-   Entre otras, ver: <http://docs.oracle.com/javase/8/docs/api/>

Para, por ejemplo, recibirlas por parámetro, debemos simplemente tipar al parámetro de nuestro método con alguna de estas interfaces.

Ejemplo:

Persona primerPersonasQueCumple(Predicate<Persona> predicado) {

`  for(Persona persona : personas) `
`    if (predicado.test(persona))`
`      return persona;`

` throw new PersonaNoExisteException();`

}

Para pasarlas por parámetro, la sintaxis es la siguiente:

`(TipoParametro parametro) -> cuerpo`

que es análogo al siguiente bloque en Smalltalk (recordar que en Smalltalk las variables no se tipan explícitamente):

` [ :parametro | cuerpo  ]`

Ejemplo:

`primerPersonaQueCumple((Persona p) -> p.esMayorDeEdad())`

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

Para más información consultar:

-   <http://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html>
-   <http://docs.oracle.com/javase/tutorial/java/javaOO/methodreferences.html>

