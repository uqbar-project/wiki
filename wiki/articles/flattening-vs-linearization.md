Cuando el comportamiento de un objeto es separado en distintas abstracciones separadas, es necesario definir cómo se resolverá la tarea de encontrar el comportamiento definido (method lookup).

Si las abstracciones son suficientemente dispares (sin comportamiento en común), pueden pensarse como complementarias, y, en consecuencia, la manera en la que se desarrolle dicha búsqueda no impactará en el comportamiento resultante. Sin embargo, cuando las abstracciones son análogas, o peor, contradictorias, el mecanismo que define el comportamiento final del objeto es un factor crítico.

Todos los lenguajes que brindan dicha modularización de abstracciones ofrecen de una u otra manera un mecanismo de búsqueda de comportamiento, de donde se destacan dos visiones principales: linearización (o “linearization”) y el aplanado (“flattening”).

La linearization es un mecanismo que define un orden para las abstracciones, priorizando unas sobre otras. A la hora de proveer un método, aquella con mayor prioridad es la que lo otorga. Todo mecanismo de linearization define un [Method Resolution Order (MRO)](http://python-history.blogspot.com.ar/2010/06/method-resolution-order.html). El caso más común es el de la herencia simple, en donde si un objeto recibe un mensaje, el método que se ejecutará es el que esté más cercano avanzando hacia lo más general en la jerarquía de herencia. Este mecanismo también es usado para la herencia múltiple y los [Mixins](mixins.html).

El flattening es un mecanismo que no prioriza abstracciones, sino que deja al usuario resolver las situaciones contradictorias o “conflictos”, mediante diferentes herramientas. Por ejemplo, los [Traits](traits.html) de Smalltalk, y la manera de lograr Trait Compositions.
