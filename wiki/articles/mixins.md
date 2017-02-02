---
layout: article
title: Mixins
---

Similares a los [Traits](traits.html), estas construcciones, que proveen la implementación de un conjunto de métodos, pueden ser combinadas y conforman un mecanismo de compartición de código complementario a las clases. Su principal diferencia con los Traits es que pueden definir y acceder a variables y su combinación no producen conflictos, ya que estos se resuelven automáticamente por un mecanismo de [linearization](flattening-vs-linearization.html).

Los Mixins tienen implementaciones en varios de los lenguajes más modernos, como ser Ruby, Scala (en el lenguaje se les dice trait, pero son mixins), Python y Groovy.

Gracias al mecanismo de linearization, los mixins resuelven el [problema del rombo o diamante](http://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem) presente en la herencia múltiple, definiendo una precedencia entre los mixins que componen una clase dependiendo del orden de composición, es decir, si la clase C se compone de los mixins {A, B}, se tomarán los métodos de B en los casos en los que se presenten conflictos. Como consecuencia de esta decisión de diseño de los mixins, resultará diferente la composición {A, B} que {B, A}.

Descripción
-----------

Un Mix-In es una subclase abstractas: como una subclase que no está ligada a ninguna superclase. Se puede "aplicar" a cualquier superclase (can be "mixed-in")

Se puede ver como una refactorización de la herencia hacia una "[chain of resposibilities](http://sourcemaking.com/design_patterns/chain_of_responsibility)".

-   En una herencia normal, cada clase (nodo) de la cadena, conoce exactamente a su siguiente (superclase). Por lo que la cadena es "rígida".
-   Un mixin es un nodo que no conoce está atado al siguiente en la cadena, aunque puede usarlo (como en el chain of resposibilities de GoF). Por lo que se puede reutilizar y aplicar a diferentes "cadenas".

Concepto
--------

-   Cumplen el rol de la reutilización que cumple una clase, sin tener el rol de ser "generadores de instancias".
-   Representan o modelan un cierto "feature" que puede ser reutilizado y aplicado a varias clases en diferentes jerarquías
-   Generalmente se utilizan para roles o características de una clase como [Observable](http://sourcemaking.com/design_patterns/observer).
-   No presentan el problema del diamante de la herencia múltiple, ya que el orden de composición actúa como técnica de resolución implícita. Es decir el primer mixin declarado que entienda el mensaje lo va a responder.

Clasificación
-------------

-   **Completo**: cuando todo su comportamiento depende de sí mismo y no de un comportamiento externo (definido en la clase sobre la que se aplica).
-   **Parcial**: cuando su comportamiento usa/delega/depende de otro comportamiento de la clase sobre la que se aplica.

Ejemplos
--------

-   [Uso de mixins en Ruby](http://www.tutorialspoint.com/ruby/ruby_modules.htm)
-   [Uso de mixins en Scala](http://docs.scala-lang.org/tutorials/tour/traits.html)
-   [Juegos de Estrategia](juegos-de-estrategia.html)

En Scala es posible, no sólo aplicar el mixin estáticamente en la jerarquía, sino también al instanciar un objeto particular. En este ejemplo vemos una alternativa al [decorator](http://sourcemaking.com/design_patterns/decorator) basado en mixins: [Stackable Trait Pattern](http://www.artima.com/scalazine/articles/stackable_trait_pattern.html)

Papers
------

[Mixin-Based Inheritance](http://stephane.ducasse.free.fr/Teaching/CoursAnnecy/0506-Master/ForPresentations/p303-bracha.pdf)
