---
layout: article
title: Uso de features de lenguajes dinamicos
---

# Terminología

El término "lenguaje dinámico" es usado en la industria con poca rigurosidad. En lugar de intentar de dar una definición precisa, señalaremos dos características típicas de estos lenguajes (aunque bien se podrían incluir muchas más):

- Presentan [chequeo de tipos dinámico e implícito](esquemas-de-tipado.html)
- Presentan facilidades para [metaprogramar](metaprogramacion.html), no limitándose a la [introspección](reflection-introspection.html) sino también haciendo uso intensivo de [auto-modificación](reflection-self-modification.html)

Es interesante marcar que la primera característica no es suficiente para hacer dinámico al lenguaje: si pudieramos reemplazar el sistema de verificación de tipos de Java para que fuera realizado enteramente en tiempo de ejecución, pero mantuviéramos su modelo de objetos y API de metaprogramación tal como la conocemos, seguiría siendo un lenguaje "estático": el tipado dinámico no es condición suficiente.

Por otro lado, el tipado dinámico no es en teoría una condición necesaria para la metaprogramación, pero sí lo simplifica notablemente: la auto-modificación en lenguajes con tipado estático requeriría de sistemas de tipos mucho más complejos que los que estamos acostumbrados a ver en la mayoría de los lenguajes.

# Features destacados

A continuación mencionamos algunos features destacados por su utilidad para metaprogramar y hacer [DSLs](dsl.html).

- Envío de mensajes "dinámicos": mandar un mensaje a un objeto a partir de su nombre
- Interceptores de código: Los lenguajes dinámicos presentan interesantes capacidades de intercepción de código, ya que ofrecen puntos en los cuales uno puede "colgarse" del mecanismo de resolución de métodos (method lookup). El más simple y usado de ellos es [method missing](method-missing.html) o does not understand: se evaluará este método siempre que se le envie un mensaje a un objeto que este no entienda. En Groovy también existe el análogo para los accessors (propertyMissing).
- Clases abiertas (open classes): Que una clase sea abierta significa que puede ser modificada luego de ser definida, para agregar, quitar o modificar cualquier aspecto de la misma (métodos, atributos, jerarquía de herencia...)
- Autoclases (Eigen Class): una clase exclusiva de esa instancia y que no se comparte con las demás permitiendo modificar la estructura y comportamiento de una instancia específica en vez de todas las de una determinada clase.
- Sintaxis flexible: Algunos lenguajes como Ruby permiten jugar con la sintaxis de modo que se pueda incrementar la expresividad del código. Algunos ejemplos de esto podría ser:
    - omitir paréntesis y puntos en el envío de mensajes
    - redefinición de operadores
    - contextualizar bloques de código de modo que se pueda elegir un receptor implícito de los mensajes

# Para profundizar y ver algunos ejemplos

- [Referencia para Ruby y Groovy](https://docs.google.com/viewer?a=v&pid=sites&srcid=ZGVmYXVsdGRvbWFpbnx1dG50YWRwfGd4OjczNjhhOWY1NjZmNDQxZjU)
