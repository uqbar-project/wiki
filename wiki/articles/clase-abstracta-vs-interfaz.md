Definiciones
------------

-   Una clase abstracta es una clase que no tiene instancias. Su utilidad consiste en proveer estructura y comportamiento común a todas las subclases que heredan de ella.

<!-- -->

-   Una interfaz permite especificar un contrato (formado por un conjunto de operaciones). Las clases que adhieren a ese contrato deben implementar esos métodos.

Comparación
-----------

Tanto la clase abstracta como la interfaz definen un tipo, pero mientras que el objetivo al definir una clase abstracta es reutilizar comportamiento, en la interfaz la idea principal es posibilitar el polimorfismo entre clases que no tienen cosas en común. No es posible definir comportamiento ni atributos para una interfaz.

Recordemos que en la mayoría de las tecnologías se trabaja únicamente con herencia simple (solamente es posible heredar de una única superclase), mientras que una clase puede tener múltiples tipos (implementar diferentes interfaces). Entonces en esos casos donde la herencia es una herramienta rígida, que permite una sola chance para ordenar la jerarquía de objetos, la interfaz puede resultar una alternativa viable.
