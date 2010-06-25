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

<code> public BigDecimal getMontoTotal() {

`   BigDecimal total = new BigDecimal(0);`
`   for (Factura factura : this.facturas) {`
`       for (Renglon renglon : this.renglones) { `
`           total = total.add(renglon.getProducto().getPrecioUnitario() * renglon.getCantidad());`
`       }`
`   }`
`   return total;`

} </code> De menos porque no tengo forma de saber el total de facturación si no se que cada factura tiene como interfaz un método que me permite saber el total (public BigDecimal getTotal())

¿Y respecto a UI? El componente de UI va a tener que conocer con el componente que maneja la lógica de dominio, de otra manera la aplicación no va a funcionar. Pero tampoco es bueno que la interfaz conozca cómo está implementado internamente un cliente, o una factura, o un empleado o un alumno. Es cierto que agregar un atributo que el usuario deba visualizar o modificar a través de la interfaz fuerza inevitablemente a un cambio en la UI, pero cambios en la lógica de negocio no deberían necesariamente impactar la UI. Así que otro de nuestros objetivos será minimizar el acoplamiento, no por ser puristas, sino porque nos traerá como beneficio no vernos impactados por cualquier tipo de cambio.
