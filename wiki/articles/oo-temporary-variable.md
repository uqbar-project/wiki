---
layout: article
title: Temporary variables
categories: [oo, bad smells, buenas prácticas, variables, temporales, efecto]
featured: true
---

## Introducción: persona instruída

Supongamos que tenemos que conocer si Helmut es una persona instruida, esto ocurre cuando leyó más de 20 libros y conoce más de 3 idiomas.

Tenemos esta implementación:

```scala
object helmut {
  var librosLeidos = 0
  const idiomasQueConoce = []
  method leerLibro() { librosLeidos = librosLeidos + 1 }
  method conocerIdioma(idioma) { idiomasQueConoce.add(idioma) }
  method esInstruido() ...
}
```

## Bad smell: temporary variable

Es un patrón frecuente la resolución del ejemplo agregando un nuevo atributo en `helmut`:

```scala
object helmut {
  ...
  var instruido = false
  method esInstruido() = instruido
}
```

Una solución similar puede ser definir instruido como una **property**, el efecto es el mismo. Esto tiene algunas desventajas:

- el problema no es tanto incorporar un nuevo atributo, sino que es un atributo que podría calcularse...
- ...y que además tiene dependencias con las otras dos variables

Para mantener consistente el estado de Helmut, deberíamos codificar su comportamiento de la siguiente manera:

```scala
object helmut {
  var librosLeidos = 0
  const idiomasQueConoce = []
  var instruido = false
  method leerLibro() {
    librosLeidos = librosLeidos + 1
    // sincronizo esInstruido...
    instruido = librosLeidos > 20 && idiomasQueConoce.size() > 3
  }
  method conocerIdioma(idioma) {
    idiomasQueConoce.add(idioma)
    // sincronizo esInstruido...
    instruido = librosLeidos > 20 && idiomasQueConoce.size() > 3
  }
  method esInstruido() = instruido
}
```

Por supuesto, podemos extraer un método aparte, igualmente estaremos enviando el mismo mensaje en ambos casos:

```scala
object helmut {
  ...
  method leerLibro() {
    librosLeidos = librosLeidos + 1
    self.actualizoInstruido()
  }
  method conocerIdioma(idioma) {
    idiomasQueConoce.add(idioma)
    self.actualizoInstruido()
  }
  method actualizoInstruido() {
    instruido = librosLeidos > 20 && idiomasQueConoce.size() > 3
  }
  ...
}
```

Otra variante similar consiste en que el método instruido haga algo como:

```scala
method leerLibro() {
  librosLeidos = librosLeidos + 1
  self.esInstruido()
}
method conocerIdioma(idioma) {
  idiomasQueConoce.add(idioma)
  self.esInstruido()
}
method esInstruido() {
  instruido = librosLeidos > 20 && idiomasQueConoce.size() > 3
  return instruido
}
```

lo cual agrega más desventajas:

- la variable `instruido` da lo mismo si es una variable de instancia (atributo) o una variable local del método `esInstruido`, solo se asigna para ser retornada
- métodos que representan acciones como leer el libro o conocer idioma, llaman a una aparente pregunta (esInstruido) solo para actualizar el estado, se confunde así métodos que tienen efecto y que no lo tienen
- si hay más atributos cuyos valores dependen de otros, puede no resultar trivial el momento de actualizar el estado del objeto sin que quede momentáneamente inconsistente (o puede resultar en errores si no respetamos el orden en que actualizamos dichos atributos)

## Una alternativa más simple

La alternativa más sencilla es descartar todos los atributos que pueden calcularse, de esa manera evitamos sincronizar el estado de Helmut:

```scala
object helmut {
  var librosLeidos = 0
  const idiomasQueConoce = []
  method leerLibro() { librosLeidos = librosLeidos + 1 }
  method conocerIdioma(idioma) { idiomasQueConoce.add(idioma) }
  method esInstruido() = librosLeidos > 20 && idiomasQueConoce.size() > 3
}
```

Aquí vemos cómo tenemos

- métodos que producen efecto (acciones): `leerLibro`, `conocerIdioma`
- y métodos que no tienen efecto (contestan preguntas): `esInstruido`

## Heurística para tener atributos que pudieran ser calculables

- si el cálculo lleva tiempo (o implica acceder a recursos externos costosos, como un archivo o un servicio web)
- si la tasa de actualización es poco frecuente pero necesitamos conocer esa información una gran cantidad de veces al día

son señales en los que tener un cálculo como atributo es justificable, algo que es improbable que ocurra en cursos iniciales de programación OO.
