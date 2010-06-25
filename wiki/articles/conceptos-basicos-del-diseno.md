Cohesión
--------

-   Una clase es cohesiva si podemos definirle un objetivo claro y puntual.
-   Un método es cohesivo si tiene un único objetivo.

Emitir una factura y calcular el total de facturación está bueno que estén en diferentes métodos. En general, tener métodos con efecto colateral (emitir factura, realizar un descuento, firmar una libreta de un alumno, cambiar el sueldo básico a un empleado) y métodos que no tengan efecto (conocer el sueldo de un empleado, saber el promedio de notas de un alumno en finales, conocer el total de facturación de un mes para un cliente, etc.) es una buena práctica, también es bueno abstraer ideas que se repiten en la misma clase dándole un nombre y dejándolo como un método aparte. Así por un lado evitamos duplicar código y por otro aumenta la cohesión de un método: se concentra en hacer sólo una cosa por vez.

Por eso mismo un Cliente representa todo lo que un cliente puede abstraer, y no tenemos objetos Empresa que acceden a los atributos de un cliente, porque así aumentamos la cohesión de nuestro sistema. De la otra manera, la Empresa toma responsabilidades de un Cliente, de un Empleado, etc. y como hace muchas cosas a la vez, el objetivo que cumple es difuso y la consecuencia de esta menor cohesión es el impacto que tiene cualquier modificación de la estructura interna de un cliente, un empleado o una factura.

Acoplamiento
------------

Es el grado en que los componentes de un sistema se conocen.

Un cliente conoce sus facturas para calcular el total, y está bien que las conozca. Lo que es nocivo para el cliente es conocer de más o de menos. De más porque si el cliente le pide las líneas (los renglones) a cada factura y luego a cada línea le pide el precio unitario de cada producto, cualquier modificación en el cálculo del precio de un producto (por ejemplo, descuento por cantidad dependiente del producto), el que se ve directamente afectado es el cliente.

<font color="#8B0000">**Ejemplo de código que produce alto acoplamiento**</font> <code>

`public BigDecimal getMontoTotal() {`
`    BigDecimal total = new BigDecimal(0);`
`    for (Factura factura : this.facturas) {`
`        for (Renglon renglon : this.renglones) { `
`            total = total.add(renglon.getProducto().getPrecioUnitario() * renglon.getCantidad());`
`        }`
`    }`
`    return total;`
`}`

</code> De menos porque no tengo forma de saber el total de facturación si no se que cada factura tiene como interfaz un método que me permite saber el total (`public` `BigDecimal` `getTotal()`)

Requerimientos y casos de uso
-----------------------------

Un requerimiento es algo que el sistema debe hacer para lograr el objetivo de un usuario. Cuando hacemos especificaciones de los casos de uso definimos la interacción entre usuario y sistema, dejando claro el límite entre lo que debe proporcionar el usuario y lo que el sistema debe responder.

**Ejemplo:** la inscripción a un examen de una materia de la facultad. *Usuario:* un alumno

1.  El usuario seleccionará la materia a inscribirse y la fecha de inscripción.
2.  El sistema validará que la materia tenga las correlativas aprobadas y que el alumno no esté anotado en otra materia en esa fecha de inscripción
3.  El sistema registrará la inscripción
4.  ... etc ...

