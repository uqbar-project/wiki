---
layout: article
title: Diferencia entre objetos y procedural   con un ejercicio de la guia 1
---

Veamos el ejercicio de la guía 1 que habla del sueldo de Pepe. El sueldo de Pepe es su sueldo básico más su bono por presentismo más otras cosas. De bono hay dos tipos

-   uno que es: 100 pesos si no faltó nunca, 50 pesos si faltó un día, 0 si faltó dos o más días.
-   otro que es siempre 0

Podrían imaginarse más bonos: uno que es 200 si faltaste hasta 2 veces y 0 si faltaste 3 o más, otro que es lo mismo pero el doble en diciembre, uno que va subiendo 10 pesos cada vez que se paga en meses consecutivos (100 el primer mes, 110 el segundo, etc., así te tentás a no faltar nunca). El ejercicio plantea dos variantes de bono x presentismo para que no se haga tan largo resolverlo, está bueno pensar que puede haber más.

Si Pepe puede tener varias variantes de bono por presentismo, no es nada lindo que el código sea algo así

calculaPresentismo: diasQueFalto

`   (bonoPorPresentismo = 1)   "el que depende de cuántos días faltó"`
`   ifTrue: [`
`         ( diasQueFalto = 0 )`
`              ifTrue:  [^100].`
`         ( diasQueFalto = 1 )`
`              ifTrue:  [^50].`
`         ^0`
`   ].`
`   (bonoPorPresentismo = 2)  "siempre 0"`
`   ifTrue: [`
`         ^0`
`   ].`
`   (bonoPorPresentismo = 3)  "otro que aparezca después"`
`   ifTrue: [`
`         ... lo que corresponda en este caso`
`   ].`

la salida en este caso es darse cuenta que conviene modelar el bono por presentismo como un objeto distinto de pepe, y que pepe tenga una variable a su bono por presentismo. Se lo seteás desde el workspace. Entonces el que calcula el valor es el bono, para lo cual necesita saber los días que faltó, entonces pepe le pregunta a su bono así:

`   bonoPresentismo importeSegun: diasFaltados`

donde `bonoPresentismo` y `diasFaltados` son variables de pepe.

Fíjense que los bonos son todos polimórficos para Pepe.

Posta es altísimamente importante que vean esto, es la papota básica de objetos: poder representar cada ente que tiene comportamiento propio (en este caso el bono x presentismo) como un objeto, y si tiene varias variantes que se comportan distinto, hacer varios objetos que sean polimórficos entre sí para los otros objetos que tengan que usar ese comportamiento. Si se entiende bien, muchas cosas (en PDP, en Diseño y después en el laburo) se simplifican.
