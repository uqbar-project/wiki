---
layout: article
title: Traits
---

Un trait permite definir un conjunto de métodos que se pueden aplicar a cualquier clase, de modo que se puedan evitar los problemas asociados a la herencia múltiple en relación a la solución de conflictos.

Características
---------------

En su definición, el conocimiento colectivo asocia a los traits con las siguientes caracteristicas:

-   Proveen un conjunto de métodos que implementan comportamiento.
-   Pueden requerir que el usuario provea un conjunto de métodos “requeridos” para su funcionamiento.
-   No pueden ser instanciados directamente.
-   No permiten definir estado interno de ninguna naturaleza.
-   No pueden acceder de forma directa al estado interno definido por el usuario.
-   Pueden ser compuestos entre ellos para generar un nuevo Trait.
-   La composición de Traits es simétrica, asociativa y conmutativa y cualquier método conflictivo es excluido de su resultado.
-   Se espera que el lenguaje provea algún mecanismo para realizar la resolución de conflictos.
-   Se resuelven por [aplanamiento o flattening](flattening-vs-linearization.html).

Sin embargo, a diferencia de las Clases (concepto que ha sido trasladado a varios lenguajes), el termino Trait es usado en varias tecnologías para referirse a construcciones similares, caracterizadas por un subconjunto de estas propiedades, lo cual vuelve borroso el concepto y hace difícil definir que puede o no esperarse de un Trait. Por ese motivo tomamos como implementación de referencia a la que se encuentra en Smalltalk, desarrollada en base al paper [Traits: Composable Units of Behaviour](http://scg.unibe.ch/archive/papers/Scha03aTraits.pdf).

Álgebra de Traits
-----------------

A continuación se detallan las operaciones que se encuentran implementadas en Smalltalk para usar traits:

**+:** Combina dos Traits en uno nuevo, el cual contiene todas las implementaciones definidas en ambos Traits.

**-:** Genera una copia del Trait destino, quitando la implementación del mensaje recibido por parámetro.

**@:** Realiza una copia de la implementación de un mensaje, utilizando otro nombre.

{% link_image Operaciones-de-Traits.png %}

### Solución de conflictos

En aquellos casos en los que una clase usa dos traits que presentan implementaciones diferentes del mismo mensaje, se produce un conflicto, que debe ser resuelto en la definición de la clase en cuestión, de lo contrario si una instancia de la misma recibe este mensaje ocurrirá un error por el conflicto sin resolver.

Las herramientas provistas por el lenguaje para la resolución de conflictos se basa en las operaciones - y @ definidas anteriormente y en la premisa de que si se define el mismo mensaje en la clase o trait en conflicto, esta definición tendrá mayor peso, con lo cual las otras definiciones dejan de usarse y el conflicto desaparece.

Por ejemplo, si la clase A usa a los traits B y C, y tanto B como C definen \#m algunas posibilidades para resolver el conflicto de \#A&gt;&gt;m son:

-   Restarle a uno de los traits el mensaje \#m, con lo cual sólo existirá una implementación, por ejemplo:

`Object subclass: #A`
`       uses: B - #m + C`
`       ...`

-   Definir la implementación real para A usando un alias para los métodos de traits, por ejemplo:

`Object subclass: #A`
`       uses: B @#{#mDeB -> #m} + C @#{#mDeC -> #m}`
`       ...`

y luego...

`#A>>m`
`  self mDeB.`
`  self mDeC.`

Si no se define \#A&gt;&gt;m además de definir los alias, el conflicto sigue existiendo.

¿Cómo diseñamos con Traits?
---------------------------

Algunas teorías dicen que todo el comportamiento debería estar en los Traits (ver paper de Ducasse citado), según esas ideas una clase se define como:

`Superclase + Trait composition (conjunción de varios traits) + Estado + Glue code `

Por otro lado existe el enfoque opuesto de modo que los Traits sólo se usan como *parches* para el modelo con herencia simple de modo que se eviten las repeticiones de código. Si sólo se busca extraer el código repetido, el Trait extraído podría no tener consistencia semántica y no conformar una entidad representativa, volviéndose difícil de reutilizar y generando abstracciones pobres.

No hay ideas muy formadas aún sobre cómo diseñar con traits y herencia simple dándoles igual peso a ambas herramientas, por lo tanto en principio, seguir el primer enfoque y a partir de ello poder descubrir mejores alternativas es una estrategia interesante para el aprendizaje.

Traits y super
--------------

La meta-variable super se usa para modificar el Method Lookup de modo que sea posible redefinir métodos heredados y a su vez reutilizar la definición de la Clase padre.

El uso de super en un método de Trait es técnicamente posible ya que estos métodos serán invocados en el contexto de las Clases usuarias donde existe una superclase de la cual se heredan definiciones de métodos. Sin embargo esto implica que el Trait sólo pueda ser usado en contextos en los cuales la superclase tenga una implementación del mensaje enviado en el método de Trait, quedando altamente acoplado con los usuarios del mismo y por lo tanto disminuyendo su reutilizabilidad.

Papers
------

-   [Traits: Composable Units of Behaviour &lt;- **el paper de Ducasse**](http://scg.unibe.ch/archive/papers/Scha03aTraits.pdf)
-   [Stateful Traits](http://scg.unibe.ch/archive/papers/Berg07aStatefulTraits.pdf)

