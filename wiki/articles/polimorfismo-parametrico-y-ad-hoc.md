Primero que nada, cuando hablamos de polimorfismo nos referimos a la capacidad de una función de recibir por parámetro valores de distinto tipo. No es taaan importante esta clasificación en sí, pero lo explicamos para despejar dudas.

Cuando hablamos de polimorfismo paramétrico tenemos una sola definición de la función, en cambio cuando tenemos polimorfismo ad-hoc tenemos muchas definiciones para la misma función de modo que se puedan soportar distintos tipos. Lo que normalmente hacemos (al menos en paradigmas) es definir funciones que son polimórficas paramétricamente, sin embargo sí usamos mucho funciones con polimorfismo ad-hoc.

Típicas funciones con polimorfismo paramétrico son las que operan sobre listas: filter, map, all, any, length, foldl, etc. No necesariamente pueden recibir "cualquier valor", depende de lo que hagas. Por ejemplo el tipo de length es

`   [a] -> Int,`

con lo cual puede recibir una lista de cualquier cosa, pero el de sum es

`   Num a => [a] -> a`

entonces sólo puede recibir una lista numérica (igual es polimórfica, porque hay muchos tipos de números, ver [ Typeclass Num](tipos-de-haskell-typeclasses.html)).

La suma también es una operación polimórfica y su tipo es:

`   Num a => a -> a -> a`

Si te fijas bien no hay forma de diferenciarlos por el tipo, lo que cambia es que la suma está definida de forma diferente para cada tipo numérico (si se animan pueden buscarlo en el [Prelude de Haskell](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Prelude.html)), mientras que el sum tiene una única definición.

Se puede ver que en ambos ejemplos de polimorfismo paramétrico una sola definición de la función sirve para todos los casos polimórficos, eso se da porque: - En el caso de length porque no hace nada específico con los elementos de la lista, entonces pueden ser cualquier cosa. - En el caso de sum porque se basa en la suma (+) que a su vez es una operación polimórfica.

Y esto último también me parece interesante, una función paramétricamente polimórfica muchas veces se basa en otra que usa polimorfismo ad-hoc (no es la única forma pero es algo frecuente).... ¿por qué es interesante esto?

Bueno primero hay que ver, ¿quién saca ventaja del polimorfismo? Y si lo pensás, las funciones que usan polimorfismo ad-hoc no sacan gran ventaja del polimorfismo, si yo tengo que definir la suma para cada tipo numérico es lo mismo que hubiera hecho sin polimorfismo.

El chiste es justamente que luego alguien basándose en eso puede hacer funciones que sirvan para todos los números, porque la suma sirve para todos los números. La suma (+) labura para el sum.

Eso es equivalente a [lo que pasa en objetos](polimorfismo-en-el-paradigma-de-objetos.html) cuando decimos que "dos objetos son polimórficos para un tercero", ¿quién saca ventaja? El tercero! - Que las golondrinas y los picaflores sean polimórficos a la hora de volar o comer no les da ventajas a las aves (ni a volar o comer), si no a los terceros que pueden hacerlos volar y comer indistintamente. - Que la suma funcione polimórficamente para reales y enteros no beneficia a la suma, sino a otras funciones que pueden sumarlos indistintamente.

En fin... volviendo a lo que decía al principio, insisto en que lo importante no es "qué tipo de polimorfismo usa una función" o "cuál es la definición de polimorfismo ad-hoc" sino, para qué sirve, qué ventajas le da a mi programa, cómo se puede aprovechar el potencial del polimorfismo para hacer código más claro/extensible/robusto/ponga aquí su cualidad preferida.
