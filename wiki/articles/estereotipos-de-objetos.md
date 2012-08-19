Un objeto presenta presentan tres características fundamentales:

-   Comportamiento: un objeto sabe hacer cosas, y eventualmente cada uno las podría saber hacer de distinta forma.
-   Estado: un objeto mantiene relaciones con otros objetos, las cuales pueden cambiar a lo largo de su vida. Muchas veces, al compararlo con paradigmas donde dato y operación son elementos claramente separados, es útil pensar al estado como justamente la parte de datos.
-   Identidad: un objeto es tratado mediante referencias, y puede ser distinguido de otro, aun cuando el otro presente el mismo estado y comportamiento.

Los objetos existen en nuestro sistema para cumplir responsabilidades. Aunque normalmente asociamos fuertemente la idea de responsabilidad a la de comportamiento hay ocasiones en que nos convendrá diseñar nuestros objetos de forma tal que expongan su estado e identidad.

En base a estos tres aspectos del objeto, podemos ensayar la siguiente caracterización de los objetos:

-   Entidades: Su identidad es importante (cuando tenemos que enviarle varios mensajes a uno de estos objetos, nos importa que el receptor sea siempre el mismo, y no tan solo uno parecido). Presentan estado significativo y mutable, y poco comportamiento, fuertemente acoplado a las características anteriores. Su cíclo de vida es típicamente largo. Son ejemplos típicos de entidades aquellos objetos persistentes en una base de datos transaccional (aunque por motivos de implementación, su identidad nativa puede perder algo de importancia, y se recurre al uso de atributos que modelan la identidad).
-   Símbolos: Su identidad es importante, no presentan comportamiento más que la comparación por identidad (==) o quizás la representación textual (toString). No presentan estado, o es inmutable. Se los emplea típicamente para modelar códigos, o elementos del metamodelo del lenguaje, como nombres de clases o selectores. Algunos lenguajes como Smalltalk o Ruby soportan nativamente estos objetos, mientras que en otros, como Java, se suple su falta con Strings
-   Valores
-   Servicios/Tareas: aquellos objetos con una fuerte carga de comportamiento independiente de su estado
-   Objetos anémicos/DTO's

| Estereotipo | Identidad    | Comportamiento | Estado            |
|-------------|--------------|----------------|-------------------|
| Entidad     | Importante   | Mucho          | Mucho y Mutable   |
| Simbolo     | Importante   | Poco           | Poco e Inmutable  |
| Valor       | Transparente | Mucho          | Mucho e Inmutable |
| Servicio    | -            | Mucho          | Poco              |
| DTO         | Importante   | Poco           | Mucho y Mutable   |

Algunas combinaciones nos llevan a: Identidad importante, estado y comportamiento inexistentes: símbolos, candados. Identidad y estado importante: entidades Comportamiento importante, estado e identidad insignificantes: objeto función, stratey stateless,

`Comportamiento ...`

Semántica de referencia vs semántica de valor

-   Su identidad no es importante
-   (Consecuencia de lo anterior) Su estado, si importante, es constante.

