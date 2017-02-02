---
layout: article
title: Variables locales en metodos
---

Se pueden usar variables locales a los métodos, lo cual puede ayudar la legibilidad o para evitar repetir la evaluación de una misma consulta a lo largo del método. Son análogas a las variables locales de procedimientos que deberían conocer de Algoritmos.

Cómo se definen y usan: ahí va un ejemplo rápido (no importa mucho lo que hace)

` miMetodoLoco: unNumero`
`   `**`|` `varLocal1` `varLocal2` `varLocal3` `|`**
`   varLocal1 := 4.`
`   varLocal2 := varLocal1 + 5.`
`   varLocal3 := varLocal1 / unNumero.`
`   ^ (varLocal1 max: varLocal2) min: varLocal3.`

En este método se definen tres variables locales entre pipes (|). Las primeras tres líneas luego de esta declaración asignan las tres variables (o sea, hacen que referencien a un objeto). Luego puedo mandarle mensajes a los objetos referenciados por esas variables y usar esas referencias tantas veces como quiera.
