¿Qué es una interfaz de usuario?
--------------------------------

Es todo lo que permite a un usuario interactuar con el sistema. Una interfaz se implementa por lo general con una pantalla, pero no es el único dispositivo posible.

Interfaz de Usuario también se puede abreviar por sus siglas en inglés: UI (User Interface)

¿Qué es el modelo de dominio?
-----------------------------

Cuando le pedimos al sistema que haga algo, hay reglas que rigen el negocio que manejamos. Si no puedo pagar con cheque a 60 días, hay una regla de negocio que lo dice. Si un alumno no puede anotarse en un final porque debe una correlativa, hay otra regla de negocio que lo dice. Si un empleado cobra un 10% del sueldo básico por presentismo, hay otra regla de negocio que lo dice. Lo que forma parte del dominio de mi aplicación es encontrar

-   un cliente que tenga un método `public` `void` `pagar(TipoPago` `tipoPago,` `BigDecimal` `monto)` donde se resuelva esa responsabilidad
-   un alumno que tenga un método `public` `void` `inscribirseAFinal(Materia` `materia)` donde se resuelva esa responsabilidad
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

-   [Cohesión](conceptos-basicos-del-diseno-cohesion.html)
-   [Acoplamiento](conceptos-basicos-del-diseno-acoplamiento.html)

¿Dónde interviene el acoplamiento al programar la UI? El componente de UI va a tener que conocer al componente que maneja la lógica de dominio, de otra manera la aplicación no va a funcionar. Pero tampoco es bueno que la interfaz defina lógica que es propia del cliente, o de la factura, o de un empleado o de un alumno (para más información ver [Interacción entre la UI y el dominio del sistema](integracion-de-la-ui-en-una-arquitectura-de-un-sistema-de-software-interaccion-entre-la-ui-y-el-dominio-del-sistema.html)). Es cierto que agregar un atributo que el usuario deba visualizar o modificar a través de la interfaz fuerza inevitablemente a un cambio en la UI, pero cambios en la lógica de negocio no deberían necesariamente afectar la UI. Así que otro de nuestros objetivos será minimizar el acoplamiento, no por ser puristas, sino porque nos traerá como beneficio no vernos impactados por cualquier tipo de cambio.

-   [Requerimientos y casos de uso](conceptos-basicos-del-diseno-requerimientos-y-casos-de-uso.html)

Una buena UI va llevando al usuario a través de la secuencia que tiene un caso de uso, guiándolo y ayudándolo en el proceso.

Diseño gráfico vs. diseño de sistemas
-------------------------------------

También es importante distinguir el diseño gráfico de una aplicación: la iconografía, los logos, imágenes de fondo, etc. que realizan los diseñadores gráficos o bien personas que no necesariamente tengan conocimientos de sistemas y que trabajan en interrelación con las personas que desarrollan un producto de software. Este diseño se preocupa por la orientación de la interfaz de manera que sea cómoda e intuitiva para el usuario y ofrezca un mínimo de resistencia o de adaptación.

Por otra parte, el diseño de sistemas se preocupa por distribuir correctamente las responsabilidades de los componentes que forman una aplicación.

Otros links relacionados
------------------------

[Algo3 Temario](algo3-temario.html)
