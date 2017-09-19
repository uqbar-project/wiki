---
layout: article
title: Ley de demeter
---

Usamos la “ley de demeter” siempre que es posible (que es la mayoría de las veces), pueden consultarla [acá](https://es.wikipedia.org/wiki/Ley_de_Demeter), un resumen:

* You can play with yourself.
* You can play with your own toys (but you can't take them apart),
* You can play with toys that were given to you.
* And you can play with toys you've made yourself.

### You can play with yourself

Esto significa que podemos enviarnos mensajes a uno mismo:

Wollok:

```wollok
object pepita {
    method graznar(){
        self.gastarEnergia()
    }
    // ...
}
```

(`self` soy yo mismo, me envío el mensaje `gastarEnergia()`)

### You can play with your own toys (but you can't take them apart)

Esto significa que nuestros atributos están para mandarles mensajes:

Wollok:

```Wollok
object pepita {
    var ciudadActual = bsAs
    method estasContenta(){
        return ciudadActual.esGrande()
    }
    // ...
}
```

(`ciudadActual` es un atributo de pepita, al que se le envía el mensaje `esGrande()`)

**Pero** no está bueno meterme con su composición interna:

```Wollok
object pepita {
    var ciudadActual = bsAs
    method estasContenta(){
        return ciudadActual.poblacion().size() > 50000 //esto es incorrecto
    }
    // ...
}
```

(es incorrecto hablar con los objetos que le pido a los objetos que le pido a los objetos.... Ya que eso es como "desarmar" a `ciudadActual`. Directamente debo pedirle lo que necesito, que es saber si es grande)

### You can play with toys that were given to you

Esto significa que puedo hablar con los **parámetros** que me lleguen

```Wollok
object pepita {
    method queresIrA(otraCiudad){
        return otraCiudad.esGrande()
    }
    // ...
}
```

(`otraCiudad` es un parámetro que me llega en este método, por eso ahora mismo puedo hablarle y mandarle el mensaje `esGrande()`)

### And you can play with toys you've made yourself

Esto significa que puedo hablar con los objetos que yo haya creado:

```Wollok
object pepita {
    method anioActual{
        return new Date().year()
    }
    // ...
}
```
(`new Date()` representa el día de hoy, que lo acabo de crear, y le mando el mensaje `year()`)

### Resumen

Puedo mandarles mensajes a self, a mis atributos, a lo que me llega por parámetro, y a los objetos que creé yo.

Entonces, esto es incorrecto:

```Wollok
object pepita {
    method queresIrA(){
        return otraCiudad.esGrande()
    }
    // ...
}
```

... porque ¿de dónde sacó la otraCiudad? No es un parámetro, no es un atributo....

### Nota sobre los WKOs

Hay muchos lenguajes que ofrecen puntos de acceso globales a ciertos objetos, con quienes también se puede hablar. 

Por ejemplo, en Smalltalk puedo hablar con las clases, que son objetos globales:

```Smalltalk
#Golondrina>>ciudadNatal
   ^ Ciudad getInstance: 'BuenosAires'
```
(Hablo con `Ciudad`, que es una clase, y le digo `getInstance:`, lo que me da una ciudad bien conocida)

Y en Wollok existen los WKOs (well known objects), que permiten hablarles directamente sin conocerlos:

```Wollok
object pepita {
    method enQueKMNaciste() {
        return buenosAires.kilometro()
    }
    // ...
}
```

(puedo hablar con `buenosAires`, aunque no sea ni un parámetro ni un atributo, ya que es un WKO)
