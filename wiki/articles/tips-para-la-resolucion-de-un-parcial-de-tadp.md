Herramientas a utilizar (o cosas que no pueden faltar)
------------------------------------------------------

-   Un diagrama de clases (que muestre una visión panorámica del sistema)
-   Código (en el que se ven los detalles)
-   Especificación de interfaces
    Interfaces entrantes: Indicar cuáles son todas las cosas que se le pueden pedir al sistema desde afuera y para cada uno de estos posibles pedidos especificar *qué mensaje debe llegarle a qué objeto* para que el sistema lleve a cabo la funcionalidad deseada.
    Interfaces salientes: Definir una interfaz Java para cada una de ellas e indicar la forma en que un objeto externo que implmente esa interfaz se puede registrar en el sistema para comenzar a interactuar con él.  

-   Diagramas adicionales
    -   Si en alguna funcionalidad intervienen muchos objetos u ocurre una cadena larga de delegaciones, es interesante mostrar una visión global de la misma en un diagrama de secuencia o de colaboración.
    -   Si un objeto o una parte del sistema pasa por varios estados diferentes puede ser interesante hacer un diagrama de estados.

: Es importante destacar que el diagrama no reemplaza al código. El diagrama provee una visión de alto nivel de la solución, para que quien tiene que leerlo tenga un primer pantallazo de la estrategia utilizada; luego es necesario bajar a los detalles y para eso es necesario verlos en el código.

-   Aclaraciones adicionales que puede ser útil incluir
    -   Justificar las decisiones, contando las alternativas que se tuvieron en cuenta y la motivación para elección realizada.
    -   Potenciales puntos débiles detectados en la solución propuesta; junto con la justificación de por qué se propone esta solución a pesar de esas dificultades señaladas.
    -   Cualquier otro comentario que ayude a comprender la solución propuesta.

