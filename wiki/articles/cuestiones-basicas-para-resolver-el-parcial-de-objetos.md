Esta es una recopilación de errores comunes a la hora de resolver un parcial de objetos. Son conceptos muy básicos, es decir, esto debe considerarse como "condiciones necesarias" para aprobar el parcial; nunca como "condiciones suficientes".

Conceptos básicos
-----------------

... tal vez alguno de los de abajo podría venir a parar a esta sección.

Cosas básicas de Smalltalk
--------------------------

Tienen la guía de lenguajes, ¡úsenla!  
Por ejemplo deberían aprovecharla para saber qué mensajes entiende una colección, no hace falta acordárselos de memoria para usarlos correctamente.

<!-- -->

No confundir mensajes con variables  
Y además ser bien explícito en el diagrama y en el código, cuándo están enviando un mensaje y cuándo están accediendo a una variable de instancia.

<!-- -->

La sintaxis es objeto-mensaje-parámetro...  
¡No da confundirse con eso!

El diagrama de clases
---------------------

¡Debe ser conexo!  
Blah, faltaría una explicación más detallada.

<!-- -->

Especificar bien las relaciones, diferenciar la herencia de la asociación o conocimiento.  
Idem

Otras cuestiones importantes
----------------------------

¡No devolver Strings para informar un error!  
Los errores se informan con self error.

<!-- -->

Indicar cómo se usa lo que hicieron  
Para cada punto del parcial, indicar qué mensaje hay que mandarle a qué objeto

<!-- -->

El orden del código es importante a la hora de entender la solución propuesta  
Está bueno ordenarlo "punto por punto" (en contraposición a juntar todos los métodos de una misma clase). No solo facilita la corrección sino además los ayuda a no olvidarse de nada

También está bueno ir "de lo general a lo particular", es decir, comenzar por el mensaje que se llamaría desde el workspace y luego los que se llaman desde ahí, así sucesivamente hasta los mensajes más específicos.

<!-- -->

Evitar repetición de código  
En el caso general no está bien tener código repetido; esto suele indicar una falta de abstracción o bien un problema de asignación de responsabilidades.


