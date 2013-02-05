Tal vez recuerden de matemática que se pueden definir conjuntos por extensión (por ejemplo {1,2,5,9}) o por comprensión ({X \\ X Є Enteros ^ X &gt; 3 ^ X &lt; 15} ). La forma por extensión ya la vimos con en Haskell con las listas \[1,2,5,9\] y por comprensión podemos definirla mediante filtros y transformaciones (filter y map) sobre otra lista que sería el conjunto base.

Las listas por comprensión en Haskell son un azúcar sintáctico que nos permite armar listas a partir de los elementos de otra luego de aplicar filtros y transformaciones, o sea, permiten escribir de una forma simple y elegante expresiones que podrían ser más complejas utilizando las funciones que ya conocemos para este fin.

Veamos un ejemplo simple usando filter y map:

`nombresDeAlumnosQueAprobaron = map nombre . filter aprobo`

Esta función puede también escribirse con una lista por comprensión:

`nombresDeAlumnosQueAprobaron alumnos = [nombre alumno | alumno <- alumnos, aprobo alumno]`

Una diferencia que podemos notar entre ambas definiciones es la cantidad de parámetros a la izquierda del igual, en el segundo caso hay uno, mientras que en el primero no hay. ¿Por qué pasa eso?

Eso pasa porque

`map nombre . filter aprobo`

es una función ya que es la composición de dos funciones.

En cambio

`[nombre alumno | alumno <- alumnos, aprobo alumno]`

es una lista, esa es una diferencia importante y es un criterio que nos va a permitir saber cuándo nos conviene usar map y filter en lugar de listas por comprensión.

Otra cosa que agregan las listas por comprensión es la posibilidad de hacer pattern matching quedando aún más expresivo:

`[nombre | (nombre, nota) <- alumnos, nota > 4]`

Otro ejemplo si tengo una lista de remeras de la forma:

`modelos = [("GoodIdeaBadIdea", "flex", 2, "negro"), ...]`

A partir de esa lista podemos construir otra usando listas por comprensión y pattern matching:

`[(nombre, color) | (nombre, _, cant, color) <- modelos, cant > 2]`

Aquí se puede ver la verdadera potencia de las listas por comprensión vs. map y filter, la posibilidad de utilizar el pattern matching.

En resumen:

-   Las listas por comprensión nos dan funcionalidades similares a las del map y filter, entonces es probable que en muchas situaciones se presenten como soluciones alternativas a un mismo problema.
-   Las listas por comprensión permiten aprovechar mejor el pattern matching, entonces en los casos donde pueda usar esa característica probablemente sea más piola usar listas por comprensión en lugar de map y filter.
-   Con listas por comprensión yo siempre defino una lista, mientras que combinando map y filter con aplicación parcial yo puedo definir funciones, eso los hace más aptos para la composición, por lo tanto en los casos en que yo necesite componer (o trabajar al nivel de función por cualquier otro motivo, puede resultar más adecuado usar map, filter, aplicación parcial, composición, etc.

