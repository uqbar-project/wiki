A diferencia de matemática y muchos lenguajes (como C o Java) en Haskell no es necesario utilizar paréntesis al pasarle los parámetros a una función. Es decir, si tenemos una función de un parámetro en matemática lo escribimos con paréntesis: ; mientras tanto, en Haskell, simplemente se pone el parámetro al lado de la función .

En el caso de tener más de un parámetro en matemática estamos acostumbrados a separarlos con comas, por ejemplo: ; en Haskell tampoco se utiliza esa sintaxis. En cambio simplemente se ponen los parámetros uno al lado del otro, separados por espacios:

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
