La pregunta se dispara en el ejercicio 2 de este final: <http://archivos-pdep.googlegroups.com/web/Final-Paradigmas-2009-02-21.pdf> Al que se propone la siguiente solución:

> Que la V.I. tipo referencie a un objeto que puede ser de las clases MateriaPrima, ProductoSemiElaborado y ProductoTerminado, y estas clases sean clases independientes, es decir que no sean subclases de ninguna ya que según el diagrama no comparten características. Entonces ahora Material deja de ser clase abstracta, para crear un material creamos un objeto de la clase Material, y a la variable tipo le asignamos un objeto de la clase de material que corresponda(MateriaPrima, ProductoSemiElaborado o ProductoTerminado).

El único detalle que yo marcaría es con respecto a los conceptos que vos mencionás. Yo creo por la forma en que usás la palabra "abstracción", lo que estás queriendo decir es "encapsulamiento".

> Aprovechariamos el concepto de polimorfismo y el de abstracción, ya que para la clase sector es transparente este cambio(abstracción), ella le va pedir el costoAlmacenamiento() a su lista de materiales y despues cada material le va a pedir a su tipo el costoAlmacenamiento(), aca se aplica el polimorfismo ya que no importa el tipo del material que sea el objeto material, todos los tipos van a entender el mensaje costoAlmacenamiento(). Y aparte esta solución permite que el dia de mañana si se quiere agregar un nuevo tipo de material solo hay que crear la clase de ese nuevo tipo y que en sus metodos de instancia este el método costoAlmacenamiento() y listo, ya se la puede referenciar con la V.I tipo. Esto es una caracteristica tmb no? pero no me acuerdo como se llama.

Tal vez es sutil la diferencia, porque se puede ver al encapsulamiento como una forma de abstracción; pero quedaría mejor tu respuesta si hacés el cambio que yo te digo.

O sea cuando vos decís "para la clase sector es transparente este cambio", eso es

-   encapsulamiento (que los clientes de un objeto no tengan que saber de su implementación interna)
-   o desacoplamiento (que el cambio a una parte de un sistema no afecte a las otras)

La clave está en la palabra transparente; La abstracción iría por el lado de encontrar una idea o un concepto que capture "la esencia" de (una parte de) mi problema y a partir de ahí me permita guiar el diseño.

La abstracción en este caso podría ser el "tipo de material", las tres clases que vos hacés polimórficas (MateriaPrima, ProductoSemielaborado, ProductoTerminado) en tu solución intentan ser tres versiones de un mismo concepto que no estaba en la solución inicial. Encontrar ese concepto es abstraer o encontrar una abstracción. Pasaste de ideas concretas (MateriaPrima, etc) a una idea más "abstracta" (tipo de material) que permite pensar a los otros como variantes de una misma cosa.

El encontrar esa abstracción es lo que te permite luego pensar en hacer a los participantes de esa abstracción polimórficos entre sí.
