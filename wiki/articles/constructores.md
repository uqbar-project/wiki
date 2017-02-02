---
layout: article
title: Constructores
---

Los constructores son métodos especiales cuya finalidad es la creación de objetos de la clase indicada con la inicialización de estado interno correspondiente. Los constructores son comunes en los lenguajes en los cuales las clases no son objetos, para invocarlos se usa la palabra reservada **new** seguido por el nombre de la clase y los parámetros según corresponda.

Por ejemplo, la clase Date de Wollok define dos constructores, uno sin parámetros que retorna la fecha del día de hoy (porque es muy común querer obtener instancias con esa configuración particular, entonces se considera como la inicialización por defecto si no se indica nada), y otro con 3 parámetros para que al instanciar la fecha se la inicialice con el día, el mes y el año deseados. La sintaxis de Wollok para instanciar objetos es:

`new Date() // retorna la fecha de hoy`
`new Date(22,10,2016) // retorna la fecha para el día, mes y año especificados.`

Esto permite que siempre que obtengamos un objeto, el mismo ya esté en condiciones de ser usado en vez de requerir que el usuario recuerde indicar los valores necesarios uno por uno mediante setters, teniendo mientras tanto un objeto con un estado inválido que no puede ser usado.

Si no definimos ningún constructor, por defecto la clase viene con un constructor vacío implícito para poder crear los objetos haciendo, por ejemplo:

`new Golondrina()`

Supongamos que queremos construir golondrinas con una cantidad de energía que nosotros elijamos como valor inicial, podríamos definir:

`class Golondrina {`
`  var energia`
`  `
`  `**`constructor`**`(_energia){`
`    energia = _energia`
`  }`
`  // ... todos los otros métodos para las golondrinas, `
`  // entre los cuales habrán muchos que trabajen con la energía asumiendo que está inicializada con algún valor numérico`
`}`

Al definir ese constructor, el constructor vacío por defecto ya no estará disponible para instanciar objetos, de esa forma sólo podremos crear objetos con un estado interno válido en base al constructor particular que hayamos definido.
