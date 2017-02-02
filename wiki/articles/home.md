---
layout: article
title: Home
---

Las Homes son objetos que representan a un conjunto de objetos. Hay situaciones donde hay requerimientos que no son propios de ningún objeto en particular sino del conjunto de los mismos. De ahí surge la necesidad de tener un objeto que permita representar ese concepto y poder así agregarle esa responsabilidad.

De allí se desprende que las Homes son objetos que tienen cierta lógica de negocio que aplica a todo un conjunto (por ejemplo en el ejercicio de Atención Médica, la HomePrestadores es un buen lugar para poner el mensaje getPrestadoresDisponibles ya que itera sobre todos los Prestador). Pero no debería convertirse en un God Object, sino que debería delegar en algún punto en otros objetos. En general deberían sólo tener la lógica que aplica a nivel de grupo para luego delegar en los objetos que contiene.

Tampoco se debe confundir la Home con el punto de entrada a todos los casos de uso. Los casos de uso deberían comenzar desde el objeto de negocio que corresponda, que puede o no ser una Home.

La idea de Home no tiene que ver con performance, o base de datos.
