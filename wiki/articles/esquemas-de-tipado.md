En resumen planteamos tres clasificaciones:

-   En cuanto a la forma de chequeo puede ser estático/compile time, dinámico/runtime o nada.
-   En cuanto a la forma de constituir un tipo puede ser nominal o estructural (no sé si agregar dependientes, también hay más variantes).
-   En cuanto a la forma de especificar el tipo de algo puede ser explícito o implícito / inferido.
-   Más complicada es la clasificación en cuanto a los tipos de polimorfismo que se banca (en particular en los lenguajes con tipado estático).

Algunos ejemplos
----------------

-   Java tiene un tipado estático, nominal y explícito (en presencia de casteos se vuelve dinámico)
-   Smalltalk es dinámico, estructural e implícito.
-   Self lo mismo.
-   Haskell es estático pero se banca ser explícito o implícito (inferencia de tipos) y en algunos casos también se comporta estructuralmente.
-   Scala es estático, tiene algo de inferencia y también soporta tipos estructurales y nominales.
-   ObjectiveC puede ser estático o dinámico si usas los ids.
-   C aparenta ser estático, pero ante casteos yo veo que el tipado es nulo.

