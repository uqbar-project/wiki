---
layout: article
title: Value object
---

*La definición de Value Object varía bastante de autor en autor. Algunas visiones diferentes sobre este concepto están plasmadas [aquí](http://c2.com/cgi/wiki?ValueObject).*

Entendemos como Value Object a aquellos objetos que tienen una semántica de valor (en contraposición con semántica de referencia), es decir cuya identidad no es importante. Dos value objects ser intercambiables, esperando iguales resultados, en tanto ambas instancias exhiban el mismo comportamiento desde el punto de vista del observador. Dicho de otra forma, son objetos para los que podría eventualmente tener una copia del mismo, y daría lo mismo enviarle un mensaje al original o su copia.

Una consecuencia importante de que los Value Object no tengan identidad importante, es que no tienen estado visiblemente mutable.

Los Value Object pueden ser también analizados como un patrón de diseño. Se recomienda el capitulo correspondiente en [este libro](http://homepages.mcs.vuw.ac.nz/~tk/fps/fps-sans-escher.pdf)

Los Value Objects pueden tener un estado visible inmutable (por ejemplo, para modelar una fecha), aunque también puede darse el caso en que tal estado no exista (por ejemplo, para modelar la función identidad) o se encuentre comletamente encapsulado (por ejemplo, una función aplicada parcialmente encapsula complementanete el estado de las variables que que fueron encerradas en su contexto)

Los Value Objet aún puede tener un estado mutable interno, en tanto este no sea expuesto a través de su interfaz, lo que habilita a que presenten evaluación diferida en en sus variables de instancia.

Impacto en el diseño
--------------------

Implementación
--------------

### Consideraciones sobre el subtipado y las relaciones de equivalencia

### En Java

`public class Final {`
`  `
`  private final String folio;`
`  private final Integer libro;`
`  private final Integer nota;`
`  private final Alumno alumno;`
`  `
`  public Final(String folio, String libro, Integer nota, Alumno alumno) {`
`    this.libro = libro;`
`    this.folio = folio;`
`    this.nota = nota;`
`    this.alumno = alumno;`
`  }`
`  public boolean estaAprobado() {`
`    return nota >= 4;`
`  }`
`  public Alumno getAlumno() {`
`    return alumno;`
`  }`

`  //demas getters`
`}`

Nótese que el estado, de existir, de un ValueObject no tiene porqué estar conformado exclusivamente por tipos primitivos (primitive obsession).

Tampoco tiene porqué estar desprovisto de comportamiento (dto/anemic object), de hecho, normalmente lo tiene y es función de su estado.

Un ValueObject puede exponer su estado interno, como también puede encapsularlo completamente (ej \[Function Object\])

### Scala

`class Final(val folio:String, val libro:Int, val nota:Int, val alumno:Alumno ) {`
`  def estaAprobado = nota >= 4`
`}`

`case class Final(val folio:String, val libro:Int, val nota:Int, val alumno:Alumno) {`
`  def estaAprobado = nota >= 4`
`}`

### En lenguajes con tipado dinámico

### en C (como un TAD)

Si bien en C no tenemos objetos, podemos también implementar TADs con semántica de valor.
