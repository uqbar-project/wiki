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

```wollok
object pepita {
    method graznar(){
        self.gastarEnergia()
    }
    // ...
}
```

### You can play with your own toys (but you can't take them apart)

Esto significa que nuestros atributos están para mandarles mensajes:

```wollok
object pepita {
    var ciudadActual = bsAs
    method estasContenta(){
        return ciudadActual.esGrande()
    }
    // ...
}
```

**Pero** no está bueno meterme con su composición interna:

```wollok
object pepita {
    var ciudadActual = bsAs
    method estasContenta(){
        return ciudadActual.poblacion().size() > 50000 //esto es incorrecto
    }
    // ...
}
```

### You can play with toys that were given to you

Esto significa que puedo hablar con los **parámetros** que me lleguen

```wollok
object pepita {
    method queresIrA(otraCiudad){
        return self.gastarEnergia()
        self.gastarEnergia()
    }
    // ...
}
```
