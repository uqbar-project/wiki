---
layout: article
title: Bloques
---

¿Qué es un bloque?
------------------

Un bloque (también conocido como closure) es un objeto, y por lo tanto podemos hacer con él lo mismo que hacíamos con los demás objetos esto es:

- Enviarle mensajes
- Pasarlo como parámetro de algún mensaje
- Hacer que una variable lo referencie

Un objeto bloque representa un cacho de código que no se ejecutó, ese código solo se ejecutará si alguien le manda un mensaje que le indique hacerlo. Veamos algunos ejemplos en el lenguaje Wollok:

Si ejecutamos las siguientes instrucciones:

```java
var x = 0
{ x = x + 1 }
```

¿A qué objeto apunta la variable x después de ejecutar esas 2 líneas de código? ¡Claro, al objeto cero (0)! Porque sólo creamos el bloque, pero el código del mismo nunca se evaluó.

Para que la variable x apunte al objeto uno (1) tenemos que decirle al bloque que se ejecute.

```java
var x = 0
{ x = x + 1 }.apply()
```

Ahora sí, después de ejecutar esas 2 líneas de código la variable x va a apuntar al objeto uno (1). Es importante darse cuenta que apply() es un mensaje que le llega al objeto bloque. Este mensaje hace que se ejecute el código que está dentro del bloque y que se retorne el objeto devuelto por la última sentencia del bloque.

Conocimiento del contexto
----------------------

Algo que se pone en evidencia en este ejemplo introductorio es que los bloques tienen una noción del contexto en el cual fueron definidos, y por eso es que tienen acceso a las referencias disponibles en dicho contexto. Es por eso que es posible usar la variable x que había sido definida fuera del bloque.

Si tuviéramos el siguiente código:

```scala
object pepita {
  var energia = 100
  
  method volar(metros) {
    energia -= 10 - metros
  }
  
  method irYVolverNVeces(metros, veces){
    veces.times({n => self.volar(metros * 2) })
  }
}
```

Y luego evaluamos: `pepita.irYVolverNVeces(5, 3)`

El mensaje `times(algoParaHacer)` que se le manda al número 3 con el bloque que construímos en ese momento se encargará de mandarle el mensaje `apply(valor)` al bloque que le pasamos, en este caso, 3 veces. Dado que la referencia metros existía en el contexto en el cual ese bloque fue creado (era un parámetro del método), es válido usar esa referencia dentro de la lógica del bloque y va a apuntar al objeto 5, como es de esperarse.

> El parámetro que se usa para aplicar el bloque será primero 1, luego 2 y finalmente 3. Esta información es irrelevante para el ejemplo, pero el bloque debe recibir un parámetro para que entienda el mensaje que recibirá cuando se evalúe el método `times`.

Otra pregunta interesante es: **¿quién es [self](self---pseudovariable.html) dentro del bloque?**

Cuando usamos self dentro de un bloque, estamos referenciando al mismo objeto que recibió el mensaje dentro del cual se creó el bloque (o sea, **pepita**), lo cual es muy convieniente ya que hace que no necesitemos parametrizar a self si necesitamos mandarle mensajes o parametrizarlo a otro mensaje dentro del código del bloque.

Finalmente, el resultado de la operación será que la energía de pepita se habrá decrementado en 60.

Bloques como funciones
----------------------

También se puede ver a los bloques como objetos que representan una función sin nombre (o sea, una función anónima, como las [Expresiones lambda](expresiones-lambda.html) de funcional!).

El bloque `{ 1 }` en Wollok es como una función constante que siempre devuelve 1 si le decís que se ejecute. Pero el chiste de las funciones es que reciban parámetros y los bloques también pueden recibir parámetros, por ejemplo la sintaxis de Wollok para un bloque de dos parámetros es `{ parametro1, parametro2 => cuerpoDelBloque }`

Ejemplos:

Podríamos representar las siguientes funciones...

`f(x) = 2x`

`g(x,y) = x2 + y2`

... con bloques de la siguiente forma:

`var f = {x => 2 * x }`

`var g = {x, y => x ** 2 + y ** 2 }`

¿Cómo hacemos para usar bloques que tienen parámetros? Por ejemplo si quisiéramos los equivalentes a evaluar las funciones f(4) y g(4,3)

`f.apply(4) // Esto devuelve el objeto 8`

`g.apply(4, 3) // Esto devuelve el objeto 25`

Por lo general el uso que le damos a los bloques es sólo la creación de los mismos para pasar por parámetro a funcionalidad ya existente de propósito general, como son los mensajes de colecciones [Mensajes de colecciones](mensajes-de-colecciones.html), y no tanto la aplicación de los mismos. Sin embargo es interesante saber cómo es que las colecciones son capaces de evaluar lo que les pedimos.

Para pensar: ¿Los mensajes que esperan por parámetro un bloque, podrán recibir por parámetro un objeto programado por nosotros? ¿Qué es lo que una colección necesita realmente que le pasemos por parámetro para poder realizar un filtrado?

¿Cómo funciona el \#ifTrue: y el \#ifFalse: de **Smalltalk**?
---------------------------------------------------------

En el caso de Smalltalk, el uso de bloques es aún más generalizado, ya que al no existir estructuras de control y tener una sintaxis completamente basada en mensajes a objetos, los bloques se usan por ejemplo para poder saber qué hacer en un condicional cuando la condición se cumple o no se cumple. Esto se logra mediante ditintos mensajes que entienden los booleanos que representan la condición sobre la cual queremos decidir.

Si en un workspace escribimos

```smalltalk
pepita energia > 0 ifTrue: [ pepita come: 30 ]
```

Pensando en términos de objeto y mensaje (mensaje = selector + parámetros), ¿qué está pasando acá?

El objeto receptor del mensaje `ifTrue:` es el objeto que me devuelve `pepita energia > 0`, ese objeto puede ser true o false.

Si el receptor es true queremos que el bloque `[ pepita come: 30 ]` se ejecute. Si el receptor es false NO queremos que el bloque `[ pepita come: 30 ]` se ejecute.

Entonces el objeto que tiene la responsabilidad de saber si el bloque debe o no ejecutarse es el booleano receptor del mensaje `ifTrue:`.

Siendo false la única instancia de la clase False y true la única instancia de la clase True, la implementación del método `ifTrue:` en cada una de las clases es

```smalltalk
True >> ifTrue: unBloque
  "self apunta a true entonces queremos que se ejecute el bloque"
  ^unBloque value
  
False >> ifTrue: unBloque
  "self apunta a false entonces NO queremos que se ejecute el bloque"
  ^nil
```

O sea que las estructuras de control a las que estábamos acostumbrados por el paradigma estructurado, no son más que mensajes polimórficos :smile:. Todo sigue las mismas reglas, objetos y mensajes.

