Modelo
------

Un modelo es una simplificación. Es una interpretación de la realidad que abstrae los aspectos relevantes para la solución de un problema. \[DDD-Eric Evans\].

Dominio
-------

Todo programa de software que se va a construir, surge como una idea, o una necesidad, que está relacionada con alguna actividad o interés. Estas "actividades" o "intereses" y el conjunto de reglas y características que lo componen, son el dominio de un problema. Cuando vayamos a construir un programa, debemos conocer y entender el dominio para poder encarar una solución al problema que tenemos. Por supuesto que la información puede ser mucha, y en algunos casos difícil de entender, por lo que debemos crear modelos que simplifique, seleccione y estructure el conocimiento de manera de enfocarlo en lo que necesitamos para solucionar el problema.

Cohesión
--------

-   Una clase es cohesiva si podemos definirle un objetivo claro y puntual.
-   Un método es cohesivo si tiene un único objetivo.

Emitir una factura y calcular el total de facturación está bueno que estén en diferentes métodos. En general, tener métodos con efecto colateral (emitir factura, realizar un descuento, firmar una libreta de un alumno, cambiar el sueldo básico a un empleado) y métodos que no tengan efecto (conocer el sueldo de un empleado, saber el promedio de notas de un alumno en finales, conocer el total de facturación de un mes para un cliente, etc.) es una buena práctica, también es bueno abstraer ideas que se repiten en la misma clase dándole un nombre y dejándolo en un método aparte. Así por un lado evitamos duplicar código y por otro aumenta la cohesión de un método: se concentra en hacer sólo una cosa por vez.

Por eso mismo un Cliente representa todo lo que un cliente puede abstraer. Si hay una clase Empresa es porque representa para nosotros una abstracción importante en el sistema, no para que la Empresa tome decisiones que son del cliente. El cliente tiene atributos + comportamiento. Así aumentamos la cohesión de nuestro sistema. De lo contrario, la Empresa toma responsabilidades de un Cliente, de un Empleado, etc. y como hace muchas cosas a la vez, el objetivo que cumple es difuso y la consecuencia de esta menor cohesión es el impacto que tiene cualquier modificación de la estructura interna de un cliente, un empleado o una factura.

Acoplamiento
------------

Es el grado en que los componentes de un sistema se conocen.

Un cliente conoce sus facturas para calcular el total, y está bien que las conozca. Lo que es nocivo para el cliente es conocer de más o de menos. De más porque si el cliente le pide las líneas (los renglones) a cada factura y luego a cada línea le pide el precio unitario de cada producto, cualquier modificación en el cálculo del precio de un producto (por ejemplo, descuento por cantidad dependiente del producto), el que se ve directamente afectado es el cliente.

<font color="#8B0000">**Ejemplo de código con alto nivel de acoplamiento**</font> <code>

`public BigDecimal getMontoTotal() {`
`    BigDecimal total = new BigDecimal(0);`
`    for (Factura factura : this.facturas) {`
`        for (Renglon renglon : factura.renglones) { `
`            total = total.add(renglon.getProducto().getPrecioUnitario() * renglon.getCantidad());`
`        }`
`    }`
`    return total;`
`}`

</code> Aquí vemos que un cliente conoce a objetos factura, pero también a renglones de factura y a productos.

Si el cliente conoce de menos no tiene forma de saber el total de facturación si no sabe que cada factura tiene como interfaz un método que me permite saber el total (`public` `BigDecimal` `getTotal()`)

<font color="#0047AB">**El mismo ejemplo con nivel adecuado de acoplamiento entre cliente y factura**</font> <code>

`public BigDecimal getMontoTotal() {`
`    BigDecimal total = new BigDecimal(0);`
`    for (Factura factura : this.facturas) {`
`        total = total.add(factura.getTotal());`
`    }`
`    return total;`
`}`

</code>

Requerimientos y casos de uso
-----------------------------

Un caso de uso es una secuencia de interacciones que se desarrollarán entre un sistema y sus actores en respuesta a un evento que inicia un actor principal sobre el propio sistema. Cuando hacemos especificaciones de los casos de uso definimos la interacción entre usuario y sistema, dejando claro el límite entre lo que debe proporcionar el usuario y lo que el sistema debe responder.

Los casos de uso pueden ser útiles para establecer requisitos de comportamiento, pero no establecen completamente los requisitos funcionales ni permiten determinar los requisitos no funcionales. Los casos de uso deben complementarse con información adicional como reglas de negocio, requisitos no funcionales, diccionario de datos que complementen los requerimientos del sistema. Sin embargo la ingeniería del funcionamiento especifica que cada caso crítico del uso debe tener un requisito no funcional centrado en el funcionamiento asociado.

Un requerimiento es algo que el sistema debe hacer para lograr el objetivo de un usuario. [ Mas sobre requerimientos](http---uqbar-wiki-org-index-php-title-conceptos-de-ingenier-c3-ada-de-software-y-de-sistemas-requerimientos.html)

Links relacionados
------------------

-   [Volver a Diseño de Sistemas](design-temario.html)

