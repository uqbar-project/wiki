¿Qué es una Interfaz de Usuario?
--------------------------------

Es todo lo que permite a un usuario interactuar con el sistema. Una interfaz se traduce por lo general a una pantalla, pero no es el único dispositivo posible.

¿Qué es el modelo de dominio?
-----------------------------

Cuando le pedimos al sistema que haga algo, hay reglas que forman el negocio que manejamos. Si no puedo pagar con cheque a 60 días, hay una regla de negocio que lo dice. Si un alumno no puede anotarse en un final porque debe una correlativa, hay otra regla de negocio que lo dice. Si un empleado tiene un 10% por presentismo, hay otra regla de negocio que lo dice. Lo que forma parte del dominio de mi aplicación es encontrar - un cliente que tenga un método public void pagar(TipoPago tipoPago) donde se resuelva esa responsabilidad - un alumno que tenga un método public void inscribirse(Materia materia) donde se resuelva esa responsabilidad - etc.

O sea, - si programamos con objetos, el modelo de dominio se compone de objetos con responsabilidades y relaciones que permiten definir los casos de uso del negocio. Más adelante volveremos sobre la idea de los casos de uso. - si programamos en otro paradigma, el modelo de dominio serán las entidades + los procesos que resuelven las cosas que necesito para la aplicación.
