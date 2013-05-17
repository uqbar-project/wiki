*Advertencia: Esta articulo tiene una cuota importante de subjetividad; no es una comparativa exacta. Así que tomalo con pinzas y recordá charlarlo con tu ayudante asignado.*

Para la materia diseño, ambas opciones son igualmente buenas. Te explico los pros (+), contras (-), y neutros (\*) de cada una:

Groovy:

-   (+) Es sintácticamente el más simple. Se parece mucho a Ruby o pseudocódigo. Usa pocas palabras reservadas y el código es muy limpio.
-   (+) El metamodelo es similar al de Java: tiene clases, métodos, herencia, etc. Agrega mixins, pero también se codifican como clases. Tiene interfaces al estilo Java, pero no son necesarias. Esto te ayudará si querés usar el segundo cuatrimestre un fmwk de persistencia o presentación para la JVM (como los dos para los que damos soporte)
-   (+) Si vos y tu grupo quieren sacarle jugo, tiene cosas muy avanzadas y podés aplicar más técnicas de diseño. Por eso es que lo estamos usando para TADP

<!-- -->

-   (\*) Tiene tipado dinámico e implícito (no escribís el tipo, los errores saltan en tiempo de ejecución). Podés usar también tipado explícito si querés (ponerle tipo a las variables), pero en la mayoría de los casos el chequeo de tipos seguirá ocurriendo en tiempo de ejecución, como en Smalltalk.
-   (\*) Se usa bastante en el mercado, aunque son pocas las empresas que lo utilizan como su lenguaje principal; se suele usar en proyectos medianos o chicos mas que grandes (hay excepciones notables, como MercadoLibre). También se lo usa como herramienta complementaria para el testing y scripting.

<!-- -->

-   (-) Eventualmente, si bajas a mucho detalle, el metamodelo de Groovy es complejo y algo inconsistente. Y ocurrirán errores muy raros (que se pueden solucionar). Pero esto normalmente ocurre cuando lo usás a su límite y jugás mucho con técnicas de metaprogramación, cosa que no es el objetivo de diseño. Por eso, esto no debería ser problema, pero igual estás advertido.
-   (-) Para entender los mensajes de error en Groovy, es recomendable entender el metamodelo de Java. Por lo cual, una leída sobre este lenguaje, sus properties (getters y setters) y sobrecarga es una buena idea.

Scala:

-   (-) Es sintácticamente mucho mas complejo. De todas formas, el código en general es muy limpio, aunque hay excepciones, sobre todo en lo que concierne a tipos.
-   (-) El metamodelo está inspirado en el de Java, pero no es idéntico. Y es más complejo. Es un híbrido de objetos y funcional. Esto puede traer algunos problemas con algunos fmwks para la JVM; de todas formas, los que usarás en la materia (hechos por nosotros), deberían andar bien de una.

<!-- -->

-   (\*) Tiene tipado estático e implícito (similar al de Haskell, tiene inferencia).Eso es bueno para detectar errores en el código pronto. De todas formas, su inferencia es más limitada que la de Haskell, por lo que tendrás que escribir más información de tipo que en Haskell, pero mucha menos que en lenguajes como Java o C.

<!-- -->

-   (+) Si tu grupo quieren sacarle el jugo, tiene cosas muy avanzadas (mucho más que Groovy). Es hoy en día uno de los lenguajes de programación más poderosos que existen y uno de los que más técnicas de diseño ofrece.
-   (+) El metamodelo, aunque complejo, es muy consistente, por lo que "todo cierra" y uno puede tener la tranquilidad de que si algo no funciona, es porque está haciendo algo mal y no porque hay un bug en el lenguaje.
-   (+) Aunque no es la panacea, en mi opinión, es uno de los lenguajes del futuro a mediano plazo (ya hay varias empresas que lo están usando de forma intensiva, ejemplo, Despegar.com).

El resumen: ambos lenguajes me parecen buenos para la materia diseño. Y de ambos te podemos dar soporte. Mi consejo: hagan algunos experimentos, bájense los entornos, y vean cual les gusta más. Algunos links:

-   Referencia de Groovy que usamos en TADP: <https://docs.google.com/viewer?a=v&pid=sites&srcid=ZGVmYXVsdGRvbWFpbnx1dG50YWRwfGd4OjczNjhhOWY1NjZmNDQxZjU>
-   Página de Scala que algunos docentes usan en la materia PHM (de San Martin): <https://sites.google.com/site/programacionhm/te/scala>

