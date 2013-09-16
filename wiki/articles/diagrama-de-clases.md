El diagrama de clases es una herramienta para [comunicar el diseño](comunicacion.html) de un programa orientado a objetos, permitiendo modelar las relaciones entre las entidades. En UML, una [clase](clases.html) es representada por un rectángulo que posee tres divisiones: Nombre de la clase, atributos que tiene y mensajes que entiende.

En el primer cuadro anotamos el nombre de la clase (si es [abstracta](herencia-clase-abstracta.html) se escribe en cursiva, o bien se usa un estereotipo &lt;<abstract>&gt; arriba del nombre de la clase).

En la segunda parte (que para nosotros no será de tanta importancia) van los atributos (o variables de instancia, las [variables de clase](variables-y-metodos-de-clase.html) van en subrayado).

En el último cuadro escribimos las operaciones (qué mensajes que puede entender). No confundir con los [métodos](mensajes-y-metodos.html) que es cómo lo resuelve cada objeto. Lo importante no es documentar todos los mensajes de un objeto, sino sólo los más relevantes. Así quedan fuera los getters, setters, métodos privados (o auxiliares) y aquellos que uno considere menores. **Moraleja:** cuidado con las herramientas que en base al código generan el diagrama (y viceversa). Bien vale la pena un diagrama útil hecho a mano antes que uno inútil en 3D.

**Importante:** Una clase que no tiene comportamiento no está comunicando qué rol cumple en la solución: o está faltando definir qué le puedo pedir o esa clase no debería estar en el diagrama.

Relaciones entre objetos
------------------------

### RELACIÓN “USA”

Dependencia: uno de los elementos usa o depende del otro cuando:

-   El objeto de clase A recibe o devuelve como parámetro de alguno de sus métodos un objeto de clase B

<!-- -->

-   Si el objeto de clase A instancia un objeto de clase B (pero no lo almacena como variable de instancia, sólo vive como variable local en el contexto de un método).

Este tipo de relación indica que los dos elementos colaboran entre sí, pero que esa relación es débil, casual; tiene un contexto temporal que no trasciende más allá de una operación. No obstante, sabemos que los cambios en la clase B podrían impactar en alguna medida en la clase A.

![](DdC-usa.png "DdC-usa.png")

### RELACIÓN “CONOCE”

Asociación: uno de los elementos conoce al otro, almacenándolo como variable de instancia.

Puede definirse una etiqueta que expresa el rol que cumple dicha relación. En cada extremo de la asociación puede agregarse la siguiente información:

-   un nombre del rol

<!-- -->

-   flechas de navegación: determina el conocimiento (navegabilidad) desde un objeto hacia el otro.

<!-- -->

-   multiplicidad: indica cuántos objetos de una clase se relacionan con la otra. La multiplicidad se puede indicar con un rango (0..1, 2..5), un rango sin cota (0..\*, 1..\*), un valor (1) o una serie de valores (1, 3, 5).

En las asociaciones, hay una relación más fuerte que en las dependencias (uso) entre ambos elementos. El conocimiento implica que la colaboración excede el marco temporal de una operación, aunque cada uno de los objetos sigue teniendo objetivos diferentes.

![](DdC-conoce.png "DdC-conoce.png")

Relaciones entre clases
-----------------------

### RELACION “HEREDA”

Generalización: una clase específica hereda los atributos, relaciones, operaciones y métodos de otra más general.

Cuando una subclase redefine el comportamiento de su superclase, se escriben los nombres de los métodos que redefine.

![](DdC-hereda.png "DdC-hereda.png")

### RELACIÓN “IMPLEMENTA”

Realización: se establece entre una clase y una [interfaz](interfaz.html); esto implica que la clase debe implementar todas las operaciones que defina la interfaz. Si bien no todos los lenguajes requieren explicitar por código la existencia de una interfaz (en Smalltalk no hace falta, pero en Java sí por tener un checkeo de tipos estático), desde un punto de vista conceptual la interfaz existe y puede comunicarse en el diagrama.

![](DdC-implementa.png "DdC-implementa.png")

Herramientas para diagramar
---------------------------

[yUML](http://yuml.me/)
