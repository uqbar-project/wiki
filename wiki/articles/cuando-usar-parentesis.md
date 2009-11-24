A diferencia de matemática y muchos lenguajes (como C o Java) en Haskell no es necesario utilizar paréntesis al pasarle los parámetros a una función. Es decir, si tenemos una función de un parámetro en matemática lo escribimos con paréntesis: ; mientras tanto, en Haskell, simplemente se pone el parámetro al lado de la función .

En el caso de tener más de un parámetro en matemática estamos acostumbrados a separarlos con comas, por ejemplo: ; en Haskell tampoco se utiliza esa sintaxis. En cambio simplemente se ponen los parámetros uno al lado del otro, separados por espacios:

Entonces los paréntesis se usan únicamente para

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

### Precedencia de los operadores más comunes en Haskell

La siguiente tabla muestra la precedencia de los operadores que más utilizamos en Haskell. A mayor número mayor precedencia. Por ejemplo, el operador tiene mayor precedencia que por lo tanto si escribimos:

`3 < 4 + 5`

se entiende como:

`3 < (4+5)`

La tabla (simplificada) es la siguiente:

`infixr 9  .`
`infixl 9  !!`
`infixr 8  ^, ^^, **`
`` infixl 7  *, /, `quot`, `rem`, `div`, `mod`, :%, % ``
`infixl 6  +, -`
`infixr 5  :`
`infixr 5  ++`
`` infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem` ``
`infixr 3  &&`
`infixr 2  ||`

Las palabras clave , e permiten indicar la *asociatividad* del operador. Los operadores definidos con asocian a izquierda, mientras que los asocian a derecha. Por lo tanto, la expresión:

`3 + 4 + 5`

se evalúa como

`(3 + 4) + 5`

ya que el operador asocia a izquierda. En cambio la expresión

`2:3:4:[]`

se debe leer como

`2:(3:(4:[]))`

ya que el operador asocia a derecha, al igual que la composición (), por ejemplo

`snd . head . filter even`

debe leerse como

`snd . (head . filter even).`

También puede notarse que todos los operadores tienen menor precedencia que la aplicación funcional, es decir que al ejemplo anterior podríamos definirlo completamente si agregamos los paréntesis alrededor de .

`snd . (head . (filter even)).`

Los operadores definidos como no son asociativos, por ejemplo el operador de igualdad . Por lo tanto la expresión:

`a == b == c`

no se entiende como ni como ; la expresión sin paréntesis es incorrecta.
