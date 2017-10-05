---
layout: article
title: Excepciones
---

Introducción
------------

Cuando un programa se ejecuta, pueden ocurrir errores: problemas de diversa índole que hacen que el sistema se comporte de una forma no esperada: el usuario ingresó un valor inválido, un objeto mal programado envió un mensaje con parámetros incorrectos, etc. Si el programa continuara ejecutándose ignorando esto, lo único que lograríamos sería que se produzcan más errores, cada vez más graves. Entonces, ¿qué hacer ante un error?

Lo más seguro es fallar, es decir, abortar el flujo de ejecución para impedir que el resto del programa continúe ejecutándose como si no hubiera pasado nada. Una forma de lograr esto es mediante el lanzamiento de excepciones.

En general cada método desarrollado debería seguir las siguientes pautas:

-   Tener un nombre descriptivo, ya que dicho nombre nos dice qué esperar, es una promesa al usuario de lo que debería suceder luego de mandar el mensaje
-   Hacer todo y sólo lo que el nombre indica
-   Si por algún motivo no se puede cumplir con lo prometido por el nombre del método, explotar de la forma más prolija posible

Es importante que aquellas cosas que puedan ser validadas para saber si no se podrá cumplir con lo prometido, se validen previamente a producir efectos colaterales, de esa forma podemos evitar algunas posibles inconsistencias en caso de poder seguir adelante manejando el problema de alguna forma.

Excepciones
-----------

Una excepción es la indicación de un problema que ocurre durante la ejecución de un programa. La principal particularidad de las excepciones es que cortan el flujo de ejecución hasta que alguien se encargue de resolverlo. Supongamos que tenemos este código Wollok:

```
object prueba {
  method msj1(){
    self.msj3(self.msj2())
  }
  
  method msj2(){
    error.throwWithMessage("Todo mal!")
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

En Wollok la forma más fácil de lanzar una excepción (de tipo Exception) es usando el WKO error como se mostró en el ejemplo anterior. Otra estrategia típica, que podemos encontrar en Smalltalk, es mediante un mensaje a self (en este caso `error:`) que está definido en Object y por ende todos los objetos entienden. Por ejemplo:

**Wollok**
```
object pepita {
  var energia = 100
  method vola(unosKms){
    if(energia < unosKms){
      error.throwWithMessage("No me da la nafta")
    }
    energia = energia - unosKms
  }
}
```

**Smalltalk**
```
pepita >> vola: unosKms
  energia < unosKms
    ifTrue: [self error: 'No me da la nafta'].
  energia := energia - unosKms 
```

En el ejemplo vemos que si la energía de pepita es menor a la cantidad de kilómetros pasados por parámetro, la operación no debería realizarse porque quedaría con energía negativa. Para evitar que eso pase se lanza el error con una descripción simpática para que el usuario o el desarrollador (dependiendo de si debería o no llegarse a esa situación) entienda qué fue lo que pasó. Lo interesante es que la línea que modifica la energía sólo llega a ejecutarse si energia >= unosKms.

Otra forma de lanzar excepciones es usando clases pertenecientes a una jerarquía particular, que son de tipo excepción. Los ejemplos anteriores podrían reescribirse de la siguiente forma:

**Wollok**
```
object pepita {
  var energia = 100
  method vola(unosKms){
    if(energia < unosKms){
      throw new Exception("No me da la nafta")  // se usa la palabra reservada throw con la excepción a lanzar
    }
    energia = energia - unosKms
  }
  ...
}
```

**Smalltalk**
```
pepita >> vola: unosKms
  energia < unosKms
    ifTrue: [Error signal: 'No me da la nafta']. "se le manda un mensaje a la clase que se encarga de romper"
  energia := energia - unosKms 
```

Esto así como está no tiene ninguna ventaja sobre lo anterior, que era bastante más bonito y simple. Para que tenga sentido, tenemos que pensar en este problema en un contexto más amplio...

¿Cómo evitar que se rompa todo ante situaciones excepcionales?
--------------------------------------------------------------

Algunos errores pueden evitarse realizando validaciones previas, pero no siempre es posible o deseable usar este enfoque. Entonces, una vez que se produce el error tenemos que tener una forma de recuperarnos del mismo para que el programa no termine con excepción.

Lo que deberíamos hacer en aquellos lugares en donde sabemos qué hacer ante un problema (que idealmente son muy pocos) es atrapar la excepción que causó el problema y evaluar un determinado código para seguir adelante de forma correcta. Para eso primero tenemos que saber qué parte del código a ejecutar es el que podría terminar en excepción, luego qué tipo de error queremos tratar y finalmente qué se debería hacer al respecto. El siguiente código va a pedirle a pepita que vuele, que puede romperse y supongamos que la forma de reaccionar ante ese problema según el requerimiento sea darle de comer para que no se muera:

**Wollok**
```
try {
  pepita.vola(100)
} catch e:Exception {
  pepita.come(50)
}
```
**Smalltalk**
``` 
[ pepita vola: 100 ] 
  on: Error do: [:error | pepita come: 50 ]
```

Un problema que tiene esta solución es que para cualquier problema se le va a dar de comer a pepita, si la energía de pepita no estuviera inicializada y el error surge de intentar tratar a la nada como un número, o si pepita no entiende el mensaje para volar, también resolvería el problema con el bloque que le da de comer en vez de romperse, de modo que sepamos que el problema existe. Y si quiero hacer cosas distintas ante problemas distintos?

Acá entra en juego lo de las clases de la jerarquía de excepción en vez de simplemente usar `self error: "..."` o el WKO `error`. Yo puedo tener clases propias que hereden de la clase de error genérica, que sean particulares del dominio en el que estoy trabajando y luego hacer algo así en Wollok `throw new NoSePuedeVolarError`, o en Smalltalk `NoSePuedeVolarError signal: 'No tengo suficiente energía para volar'`.

Eso permite atrapar sólo las excepciones que me interesan y dejar pasar las que no sé cómo manejar para que alguien más se ocupe. Por ejemplo:

**Wollok**
```
try {
  pepita.vola(100)
} catch e:NoSePuedeVolarError {
  pepita.come(50)
}
```
**Smalltalk**
``` 
[ pepita vola: 100 ] 
  on: NoSePuedeVolarError do: [:error | pepita come: 50 ]
```

También, si estamos testeando podemos verificar que el resultado de ejecutar algo sea no poder volar:

**Wollok**

`assert.throwsExceptionWithType(new NoSePuedeVolarError(), {pepita.vola(100)})`

**Smalltalk**

`self should: [ pepita vola: 100] raise: NoSePuedeVolarError`

Al usar `throwsExceptionWithType` o `should:raise:` el test va a dar verde exclusivamente si el bloque al ejecutarse lanza una excepcion cuya clase sea NoSePuedeVolarError o alguna subclase de la misma. Si el error que lanza ejecutar ese bloque es por ejemplo que un objeto no entendió un mensaje como se explicó antes, el test daría rojo :D

Estrategias para manejar excepciones
------------------------------------

-   La forma por excelencia de lidiar con una excepción es **no hacer nada!!**. La mayoría de las veces no tenemos la capacidad de recuperarnos del problema en el mismo lugar donde se produce, lo más sano es dejarla burbujear hasta el punto en donde sí haya algo para hacer al respecto.
-   Atraparla, hacer algo y continuar con el flujo normal de ejecución.
-   Atraparla, hacer algo y volver a lanzar la misma excepción. Eso se puede hacer volviendo a usar `throw` en Wollok o en Smalltalk mandando `signal` a la excepcion atrapada en el bloque que maneja el problema.
-   Atraparla y lanzar otra más adecuada agregando más información del problema.

Pueden leer un poquito más sobre este tema [acá](manejo-de-errores.html) (los ejemplos están en Java)
