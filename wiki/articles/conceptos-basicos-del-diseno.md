Cohesión
--------

-   Una clase es cohesiva si podemos definirle un objetivo claro y puntual.
-   Un método es cohesivo si tiene un único objetivo.

Emitir una factura y calcular el total de facturación está bueno que estén en diferentes métodos. En general, tener métodos con efecto colateral (emitir factura, realizar un descuento, firmar una libreta de un alumno, cambiar el sueldo básico a un empleado) y métodos que no tengan efecto (conocer el sueldo de un empleado, saber el promedio de notas de un alumno en finales, conocer el total de facturación de un mes para un cliente, etc.) es una buena práctica, también es bueno abstraer ideas que se repiten en la misma clase dándole un nombre y dejándolo como un método aparte. Así por un lado evitamos duplicar código y por otro aumenta la cohesión de un método: se concentra en hacer sólo una cosa por vez.

Por eso mismo un Cliente representa todo lo que un cliente puede abstraer, y no tenemos objetos Empresa que acceden a los atributos de un cliente, porque así aumentamos la cohesión de nuestro sistema. De la otra manera, la Empresa toma responsabilidades de un Cliente, de un Empleado, etc. y como hace muchas cosas a la vez, el objetivo que cumple es difuso y la consecuencia de esta menor cohesión es el impacto que tiene cualquier modificación de la estructura interna de un cliente, un empleado o una factura.
