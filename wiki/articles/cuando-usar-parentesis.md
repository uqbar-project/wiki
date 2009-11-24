A diferencia de matemática y muchos lenguajes (como C o Java) en Haskell no es necesario utilizar paréntesis al pasarle los parámetros a una función. Es decir, si tenemos una función f en matemática escribimos:

`f(5)`
`g(3,4)`

en haskell escribiremos:

`f 5`
`g 3 4`

Entonces los paréntesis se usan para

-   Alterar la precedencia
-   Tuplas
-   Usar operadores sin aplicarles todos los parámetros, caso típico (+) o (+1).

Alterar la precedencia
----------------------

Antes que nada hay que entender qué significa el [Concepto de Precedencia](concepto-de-precedencia.html) en un lenguaje de programación.

Los paréntesis permiten naturalmente alterar la precedencia propia del lenguaje, por ejemplo si evaluamos

`2 + 3 * 4`

obtendremos como resultado 14, ya que la multiplicación tiene mayor precedencia que la suma; utilizando paréntesis podemos alterar ese comportamiento, si escrigimos

`(2 + 3) * 4`

podemos lograr que la operación de suma se ejecute "antes" que la multiplicación y obtener 20 como resultado.
