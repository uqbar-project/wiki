Esta es una recopilación de errores comunes a la hora de resolver un parcial de objetos. Son conceptos muy básicos, es decir, esto debe considerarse como "condiciones necesarias" para aprobar el parcial; nunca como "condiciones suficientes".

Conceptos básicos
-----------------

Utilizar los conceptos del paradigma  
Encontrar abstracciones, aprovechar el polimorfismo, delegar responsabilidades, etc.

Hay que programar con objetos, no vale programar como si fuera Algoritmos... para aprender eso ya está Algoritmos.

<!-- -->

Repartir las responsabilidades  
Que no me quede todo el código en un mismo objeto. Es fácil verlo (por ejemplo) cuando una caja del diagrama de clases ocupa la mitad de la página.

Que no todos los mensajes del workspace vayan al mismo objeto.

Que no haya un objeto "Sistema".

<!-- -->

Ser consistente  
Por ejemplo:

-   Si en algún lugar mando un mensaje y trato el resultado como una colección debo asegurarme de que la implementación del método devuelva una colección (lo mismo vale para colecciones como para cualquier otro objeto).
-   Si tengo muchos métodos polimórficos todos deben recibir los mismos parámetros y devolver objetos del mismo tipo.
-   En un método puedo usar las variables de instancia del objeto o los parámetros, cualquier otra variable "mágica" que aparezca constituye un error.

<!-- -->

Ser específico  
Indicar las relaciones de conocimiento en el diagrama de objetos.

También los atributos que tiene cada objeto.

También tiene que quedar claro de qué tipo es cada parámetro que espera un método, y qué devuelve.

<!-- -->

Elegir buenos nombres para clases, métodos y variables  
Por ejemplo diferenciar desde el nombre los métodos que son órdenes de los que son consultas. Para los primeros conviene usar oraciones en infinitivo o en imperativo. Para las consultas en general usamos el indicativo:

-   "caminá" o "caminar" podrían indicar la orden de realizar una caminata (en el parcial vale poner el acento para que se note la intención).
-   "caminaste" podría ser una consulta que devuelve un booleano indicando si caminó
-   "cuantoCaminaste" podría ser una consulta que devuelve una cantidad.
-   Por otro lado, un nombre como "caminata" dificulta comprender la intención.

Esto es aún más importante si el método en cuestión no está 100% correcto, ya que al menos permite entender lo que quisieron hacer.

Cosas básicas de Smalltalk
--------------------------

Si bien no es nuestro objetivo saber bocha de Smalltalk, hay cositas que impactan directamente en la posibilidad o no de comprender lo que están programando; entonces conviene darles bola:

Tienen la guía de lenguajes, ¡úsenla!  
Por ejemplo deberían aprovecharla para saber qué mensajes entiende una colección, no hace falta acordárselos de memoria para usarlos correctamente.

<!-- -->

No confundir mensajes con variables  
Y además ser bien explícito en el diagrama y en el código, cuándo están enviando un mensaje y cuándo están accediendo a una variable de instancia.

<!-- -->

La sintaxis es objeto-mensaje-parámetro...  
¡No da confundirse con eso!

<!-- -->

Identificar bien mayúsculas y minúsculas.  
Los nombres de clase van con mayúscula, al igual que las variables de clase.

Todo lo demás va en minúscula.

Romper estas reglas obliga a que el corrector tenga dudas de lo que están haciendo.

El diagrama de clases
---------------------

¡Debe ser conexo!  
No vale sólo dibujar las cajitas con los métodos y atributos, las relaciones entre los objetos son igual de importantes. Una clase que no se relaciona con nadie no aporta a la solución porque nadie puede mandarle mensajes a sus instancias. Si sólo se comunican con otros objetos por parámetros se dibuja una flecha con línea punteada con la palabrita <usa> que parte del receptor del mensaje en cuestión y llega al objeto que se envía por parámetro.

<!-- -->

Especificar bien las relaciones, diferenciar la herencia de la asociación o conocimiento.  
Si la flecha que se usa para los atributos tiene un triángulo en la punta o si la de herencia es una flecha simple está mal!

La presentación del parcial
---------------------------

Indicar cómo se usa lo que hicieron  
Para cada punto del parcial, indicar qué mensaje hay que mandarle a qué objeto

<!-- -->

El orden del código es importante a la hora de entender la solución propuesta  
Está bueno ordenarlo "punto por punto" (en contraposición a juntar todos los métodos de una misma clase). No solo facilita la corrección sino además los ayuda a no olvidarse de nada

También está bueno ir "de lo general a lo particular", es decir, comenzar por el mensaje que se llamaría desde el workspace y luego los que se llaman desde ahí, así sucesivamente hasta los mensajes más específicos.

<!-- -->

Prolijidad  
Hagan buena letra, no amontonen todo, etc.

<!-- -->

No escribir muy apretado  
Así hay lugar para hacer anotaciones y correcciones.

<!-- -->

Ortografía  
Los docentes tenemos indicación explícita del departamento de desaprobar a quienes tengan muchos errores de ortografía, así que pilas con eso.

De paso no estaría mal practicar un poquito de redacción, aunque eso va más que nada para el final.

Otras cuestiones importantes
----------------------------

¡No devolver Strings para informar un error!  
Los errores se informan con self error.

<!-- -->

Evitar repetición de código  
En el caso general no está bien tener código repetido; esto suele indicar una falta de abstracción o bien un problema de asignación de responsabilidades.


