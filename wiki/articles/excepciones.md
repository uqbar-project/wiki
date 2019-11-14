---
layout: article
title: Excepciones
---

Introducción
------------

Cuando un programa se ejecuta, pueden ocurrir errores: problemas de diversa índole que hacen que el sistema se comporte de una forma no esperada: el usuario ingresó un valor inválido, un objeto mal programado envió un mensaje con parámetros incorrectos, etc. Si el programa continuara ejecutándose ignorando esto, lo único que lograríamos sería que se produzcan más errores, cada vez más graves. Entonces, ¿qué hacer ante un error?

Lo más seguro es fallar, es decir, abortar el flujo de ejecución para impedir que el resto del programa continúe ejecutándose como si no hubiera pasado nada. Una forma de lograr esto es mediante el lanzamiento de excepciones.

En general cada método desarrollado debería seguir las siguientes pautas:

- Tener un nombre descriptivo, ya que dicho nombre nos dice qué esperar, es una promesa al usuario de lo que debería suceder luego de mandar el mensaje
- Hacer todo y sólo lo que el nombre indica
- Si por algún motivo no se puede cumplir con lo prometido por el nombre del método, explotar de la forma más prolija posible

Es importante que aquellas cosas que puedan ser validadas para saber si no se podrá cumplir con lo prometido, se validen previamente a producir efectos colaterales, de esa forma podemos evitar algunas posibles inconsistencias en caso de poder seguir adelante manejando el problema de alguna forma.

Excepciones
-----------

Una excepción es la indicación de un problema que ocurre durante la ejecución de un programa. La principal particularidad de las excepciones es que cortan el flujo de ejecución hasta que alguien se encargue de resolverlo. Supongamos que tenemos este código Wollok:

```Wollok
object prueba {
  method msj1(){
    self.msj3(self.msj2())
  }
  
  method msj2(){
    self.error("Todo mal!")
    return "Esto no se va a ejecutar nunca"
  }
  
  method msj3(string) = "Opa! " ++ string
}
```

De esta forma si el objeto que define esto recibe el mensaje msj1, la excepción lanzada en msj2 cortará la ejecución con lo cual no se evaluará la siguiente línea ni se mandará msj3. Es correcto dejar que la excepción se propague hacia atrás por la cadena de mensajes enviados siempre que no haya nada para hacer al respecto. Eventualmente, en algún punto donde sí sea posible tomar alguna acción, se podrá manejar esta excepción y continuar la ejecución con normalidad.

Bugs vs Errores de usuario
--------------------------

Algunos errores surgen por un bug en el programa, por ejemplo si un objeto no entiende un mensaje la forma de resolverlo es modificar el código para que o bien lo entienda o el mismo no le llegue dependiendo de si debería o no entenderlo. Por ejemplo, si a un Set le pedimos el primer elemento tira un error porque no es una colección ordenada, por ende no debe responder al mensaje first como sí lo hace una colección ordenada.

Otros errores surgen del uso del programa, ya que pueden darse situaciones que llevan a que un objeto no pueda realizar lo que se le pide. Por ejemplo, si a una colección vacía el mandamos el mensaje anyOne tira un error porque no tiene forma de resolver el problema.

Lanzando Excepciones
--------------------

En Wollok la forma más fácil de lanzar una excepción es mediante un mensaje a self (en este caso `error(descripcion)`) que todos los objetos entienden. Por ejemplo:

```Wollok
object pepita {
  var energia = 100
  method vola(unosKms){
    if(energia < unosKms){
      self.error("Energía insuficiente para volar los kilómetros requeridos")
    }
    energia = energia - unosKms
  }
}
```

En el ejemplo vemos que si la energía de pepita es menor a la cantidad de kilómetros pasados por parámetro, la operación no debería realizarse porque quedaría con energía negativa. Para evitar que eso pase se lanza el error con una descripción simpática para que el usuario o el desarrollador (dependiendo de si debería o no llegarse a esa situación) entienda qué fue lo que pasó. Lo interesante es que la línea que modifica la energía sólo llega a ejecutarse si energia >= unosKms.

Otra forma de lanzar excepciones es usando clases pertenecientes a una jerarquía particular, que son de tipo excepción. El ejemplo anterior podría reescribirse de la siguiente forma, usando `DomainException` que es el mismo tipo de excepción que usa el mensaje error por atrás y hereda de la clase `Exception`:

```Wollok
object pepita {
  var energia = 100
  method vola(unosKms){
    if(energia < unosKms){
      throw new DomainException(message = "Energía insuficiente para volar los kilómetros requeridos")  // se usa la palabra reservada throw con la excepción a lanzar
    }
    energia = energia - unosKms
  }
  ...
}
```

Esto así como está no tiene ninguna ventaja sobre lo anterior, que era bastante más bonito y simple. Para que tenga sentido, tenemos que pensar en este problema en un contexto más amplio...

¿Cómo evitar que se rompa todo ante situaciones excepcionales?
--------------------------------------------------------------

Algunos errores pueden evitarse realizando validaciones previas, pero no siempre es posible o deseable usar este enfoque. Entonces, una vez que se produce el error tenemos que tener una forma de recuperarnos del mismo para que el programa no termine con excepción.

Lo que deberíamos hacer en aquellos lugares en donde sabemos qué hacer ante un problema (que idealmente son muy pocos) es atrapar la excepción que causó el problema y evaluar un determinado código para seguir adelante de forma correcta. Para eso primero tenemos que saber qué parte del código a ejecutar es el que podría terminar en excepción, luego qué tipo de error queremos tratar y finalmente qué se debería hacer al respecto.

El siguiente código va a pedirle a pepita que vuele, que puede romperse y supongamos que la forma de reaccionar ante alguna excepción según el requerimiento sea darle de comer para que no se muera:

```Wollok
try {
  pepita.vola(100)
} catch e : Exception {
  pepita.come(50)
}
```

Un problema que tiene esta solución es que para cualquier problema se le va a dar de comer a pepita, si la energía de pepita no estuviera inicializada y el error surge de intentar tratar a la nada como un número, o si pepita no entiende el mensaje para volar, también resolvería el problema con el bloque que le da de comer en vez de romperse, de modo que sepamos que el problema existe. Eso lógicamente no es correcto.

Considerando que el mensaje `self.error(descripcion)` lanza una excepción más particular que Exception, una primer mejora que se puede hacer es acotar ante qué tipo de error queremos darle de comer a pepita:

```Wollok
try {
  pepita.vola(100)
} catch e : DomainException {
  pepita.come(50)
}
```

Eso va a evitar que nos recuperemos incorrectamente de errores como ser mensajes no entendidos, ya que no son de tipo DomainException, sin embargo sería interesante poder refinarlo un poco más, para tener la certeza de que sólo vamos a estar manejando con este mecanismo las excepciones que surjan por tener energía insuficiente.

Acá entra en juego nuevamente la jerarquía de clases de excepción. En vez de simplemente usar `self.error(descripcion)`, podríamos tener clases propias que hereden de la clase de excepción más apropiada (en este caso `DomainException` podría tener sentido):

```Wollok
class EnergiaInsuficienteException inherits DomainException {}
```

...y luego hacer algo así en el método `vola(kms)` de pepita: `throw new EnergiaInsuficienteException(message = "Energía insuficiente para volar los kilómetros requeridos")`.

Eso permite atrapar sólo las excepciones que me interesan y dejar pasar las que no sé cómo manejar para que alguien más se ocupe. Por ejemplo:

```Wollok
try {
  pepita.vola(100)
} catch e:EnergiaInsuficienteException {
  pepita.come(50)
}
```

También, si estamos testeando podemos verificar que el resultado de ejecutar algo sea no poder volar:

```Wollok
assert.throwsExceptionWithType(new EnergiaInsuficienteException(), {pepita.vola(100)})
```

Al usar `throwsExceptionWithType` el test va a dar verde exclusivamente cuando el bloque al ejecutarse lanza una excepcion cuya clase sea `EnergiaInsuficienteException` o alguna subclase de la misma. Si el error que lanza ejecutar ese bloque es por ejemplo que un objeto no entendió un mensaje como se explicó antes, el test no va a dar verde, y es exactamente lo que necesitamos.

Estrategias para manejar excepciones
------------------------------------

- La forma por excelencia de lidiar con una excepción es **no hacer nada!!**. La mayoría de las veces no tenemos la capacidad de recuperarnos del problema en el mismo lugar donde se produce, lo más sano es dejarla burbujear hasta el punto en donde sí haya algo para hacer al respecto.
- Atraparla, hacer algo y continuar con el flujo normal de ejecución.
- Atraparla, hacer algo y volver a lanzar la misma excepción. Eso se puede hacer volviendo a usar `throw` usando la excepcion atrapada en el bloque que maneja el problema.
- Atraparla y lanzar otra más adecuada agregando más información del problema. Las excepciones además de un mensaje descriptivo pueden tener asociada una *causa* que es otra excepción y sirve justamente para los casos en los cuales se usa esta estrategia. Si vemos stacktrace completo de una excepción que tiene una instancia de otra excepción como causa, podremos notar que se incluye la información de ambas excepciones, sin perder información por el camino.

¿Donde es común atrapar excepciones?
------------------------------------

Lo más usual es que las excepciones no se atrapen en el código de nuestros objetos de dominio, sino mucho más lejos, en unos pocos lugares puntuales. Por ejemplo, si tenemos un sistema con una interfaz gráfica que es la que el usuario final usa para interactual, vamos a querer que en esa interfaz se muestre un mensaje razonable para que el usuario vea. Por ejemplo, si el problema está relacionado con alguna acción incorrecta por parte del usuario, lo ideal sería comunicársela de modo que pueda entender el problema y corregirlo; si en cambio es un problema inesperado sobre el cual el usuario no puede hacer nada (un problema del programa en sí, no del uso), se le podría mostrar otro tipo de mensaje para que sepa que hubo un problema y también informar del problema a los desarrolladores para que puedan analizarlo y trabajar sobre la causa.

En las herramientas de testeo que **usan** nuestro código también de seguro se están atrapando las excepciones que se lancen al ejecutar nuestra lógica, lo cual lleva a que se reporte adecuadamente si las pruebas pasaron, si falló una aserción o si hubo un error inesperado, y la información de esas excepciones que se atraparon debería poder ser vista por quien corre las pruebas.

Eso no quiere decir que nunca se atrapen excepciones dentro del modelo, sin embargo hay que entender que no es tan común, y es importante no abusar de estas herramientas, ya que podrían traer más problemas que soluciones.
