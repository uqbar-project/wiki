¿Qué es una Interfaz de Usuario?
--------------------------------

Es todo lo que permite a un usuario interactuar con el sistema. Una interfaz se traduce por lo general a una pantalla, pero no es el único dispositivo posible.

¿Qué es el modelo de dominio?
-----------------------------

Cuando le pedimos al sistema que haga algo, hay reglas que rigen el negocio que manejamos. Si no puedo pagar con cheque a 60 días, hay una regla de negocio que lo dice. Si un alumno no puede anotarse en un final porque debe una correlativa, hay otra regla de negocio que lo dice. Si un empleado tiene un 10% por presentismo, hay otra regla de negocio que lo dice. Lo que forma parte del dominio de mi aplicación es encontrar

-   un cliente que tenga un método `public` `void` `pagar(TipoPago` `tipoPago)` donde se resuelva esa responsabilidad
-   un alumno que tenga un método `public` `void` `inscribirse(Materia` `materia)` donde se resuelva esa responsabilidad
-   etc.

O sea,

-   si programamos con objetos, el modelo de dominio se compone de objetos con responsabilidades y relaciones que permiten definir los casos de uso del negocio.
-   si programamos en otro paradigma, el modelo de dominio serán las entidades + los procesos que resuelven las cosas que necesito para la aplicación.

¿Qué objetivos nos proponemos al programar una interfaz de usuario?
-------------------------------------------------------------------

Por supuesto que ande, pero además vamos a priorizar ciertas cualidades de diseño. En particular tratar de no mezclar ideas de presentación con negocio. O sea, separar la lógica para definir la interacción con el usuario y la lógica propia del dominio. ¿Por qué?

-   porque no quiero que mi dominio se vea afectado por cuestiones tecnológicas.
-   porque eso me lleva a perder cohesión en los objetos de presentación, que además de encargarse de mostrar la información tienen que atacar cuestiones de negocio
-   porque tengo más restricciones a nivel usuario y tecnológicos del lado de la UI (es la parte más compleja y la menos madura, cuesta encontrar buenas abstracciones)

[Conceptos básicos del diseño](conceptos-basicos-del-diseno.html)
-----------------------------------------------------------------------

-   [Cohesión](conceptos-basicos-del-diseno.html)
-   [Acoplamiento](conceptos-basicos-del-diseno.html)

¿Qué pasa con el acoplamiento respecto a la UI? El componente de UI va a tener que conocer al componente que maneja la lógica de dominio, de otra manera la aplicación no va a funcionar. Pero tampoco es bueno que la interfaz conozca cómo está implementado internamente un cliente, o una factura, o un empleado o un alumno. Es cierto que agregar un atributo que el usuario deba visualizar o modificar a través de la interfaz fuerza inevitablemente a un cambio en la UI, pero cambios en la lógica de negocio no deberían necesariamente impactar la UI. Así que otro de nuestros objetivos será minimizar el acoplamiento, no por ser puristas, sino porque nos traerá como beneficio no vernos impactados por cualquier tipo de cambio.

-   [Requerimientos y casos de uso](conceptos-basicos-del-diseno.html)

Una buena UI va llevando al usuario a través de la secuencia que tiene un caso de uso, guiándolo y ayudándolo en el proceso.

**Ejemplo:** la inscripción a un examen de una materia de la facultad. *Usuario:* un alumno

1.  El usuario seleccionará la materia a inscribirse y la fecha de inscripción.
2.  El sistema validará que la materia tenga las correlativas aprobadas y que el alumno no esté anotado en otra materia en esa fecha de inscripción
3.  El sistema registrará la inscripción
4.  ... etc ...

