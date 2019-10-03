---
layout: article
title: Objetos factory - instanciando objetos
featured: true
categories: [oo, factory, instanciación, construcción, creación]
---

# Introducción

Instanciar objetos no siempre resulta fácil, especialmente en los tests, cuando debemos tener en cuenta:

- valores por defecto para esa clase de equivalencia
- configuraciones para algunos tests específicos
- tratando siempre de que sea fácil de usar y no repetir código

Por ejemplo, necesitamos definir una computadora que tiene 16 GB de RAM y un disco HDD de 2048, con mouse, en base a esta definición:

```scala
class SSD {
  method precio() = 500
  method estaCopado() = true
}

class HDD {
  var property tamanio = 1024   // expresado en GB
  
  method precio() = if (tamanio > 1024) 400 else 250
  method estaCopado() = tamanio > 1024
}

class Memoria {
  var property tamanio          // expresado en GB
  
  method precio() = tamanio / 10
  method estaCopada() = tamanio > 16
}

class Computadora {
  var memoria
  var disco = new HDD()
  var mouse = true

  method precio() = memoria.precio() + disco.precio() + 1000
  method estaCopada() = memoria.estaCopada() || disco.estaCopado()
}
```

## Clases de equivalencia para el precio

Las clases de equivalencia para el precio se forman en base a diferentes configuraciones de computadoras

- ssd, no importa la memoria
- hdd de más de 1TB, no importa la memoria
- hdd de 1TB, no importa la memoria
- hdd de menos de 1TB, no importa la memoria

La presencia o no del mouse no juega ningún papel, y realmente tampoco la memoria que calcula siempre el mismo valor.

## Clases de equivalencia para estaCopada

En cambio, las clases de equivalencia que configuran a una computadora copada son:

- computadora con ssd, no importa la memoria (copada)
- computadora con hdd de más de 1TB, no importa la memoria (copada)
- computadora con hdd de 1TB, memoria de 16 (no copada - caso borde en ambas configuraciones)
- computadora con menos de 1TB, memoria más de 16 (copada)
- computadora con menos de 1TB, memoria de menos de 16 (no copada)

## Unificando las clases de equivalencia

Ahora vemos que si intersectamos las clases de equivalencia para precio y estaCopada, las primeras 4 de estaCopada coinciden con las de precio, por lo tanto vamos a tener que buscar estas configuraciones:

```scala
  var computadoraConSsd      = new Computadora()
  var computadoraConPocoHdd  = new Computadora(disco = new HDD(tamanio = 512))
  var computadoraComun       = new Computadora(disco = new HDD(tamanio = 1024))
  var computadoraConMuchoHdd = new Computadora(disco = new HDD(tamanio = 2048))
```

# Dónde lo implementamos

Las opciones son varias:

- en cada test configuramos la computadora que necesitamos: eso tiene la desventaja de que si es necesario resolver la inicialización en más pasos, se empezarán a repetir en cada uno de los tests.
- lo ubicamos en el describe como variables, eso tiene dos desventajas: a) los tests tienen pocas variables compartidas entre sí (baja cohesión), no es fácil ver qué tests usan qué variables y la inicialización también está lejos del momento en que se prueban, y b) cada vez que corremos un test se evalúan todas las expresiones del describe, por lo que hay una penalización en performance.
- delegar en otro objeto la inicialización de las clases de equivalencia que necesitamos, algo como

```scala
object computadoraFixture {
  method computadoraConSSD() =
    new Computadora()
  
  method computadoraConPocoHdd() =
    new Computadora(disco = new HDD(tamanio = 512))
  
  method computadoraComun() =
    new Computadora(disco = new HDD(tamanio = 1024))
  
  method computadoraConMuchoHdd() =
    new Computadora(disco = new HDD(tamanio = 2048))

}
```

Es decir, tenemos un objeto que nos ayuda a crear otros objetos, lo que podríamos llamar **objetos factory** (fabrican configuraciones de objetos que nosotros necesitamos frecuentemente).

## Ventajas en el mantenimiento

El costo que pagamos es mantener una nueva abstracción, la ventaja es que si la computadora tuviera ahora una lista de dueños:

```scala
object juan {
  method esCopado() = ...
}

object ceci {
  method esCopado() = ...
}

class Computadora {
  var memoria
  var disco = new HDD()
  var mouse = true
  const duenios = []

  method precio() = memoria.precio() + disco.precio() + 1000

  method agregarDuenio(duenio) {
    duenios.add(duenio)
  }

  method estaCopada() =
    memoria.estaCopada() || disco.estaCopado() || duenios.any { duenio => duenio.esCopado() }
}
```

Y necesitáramos agregar en la computadora común a dos dueños, solo debemos modificar un método:

```scala
  method computadoraComun() {
    const compu = new Computadora(disco = new HDD(tamanio = 1024))
    compu.agregarDuenio(ceci)
    compu.agregarDuenio(juan)
    return compu
  }
```

También podríamos hacer que los otros métodos se llamen entre sí para agregar los duenios:

```scala
method computadoraConMuchoHdd() {
  const compu = self.computadoraComun()
  compu.disco(new HDD(tamanio = 2048))
  return compu
}
```

Esto también podemos hacerlo con la variante del fixture, solo que cada vez que ejecutemos cada test se evaluarán todos los métodos.
