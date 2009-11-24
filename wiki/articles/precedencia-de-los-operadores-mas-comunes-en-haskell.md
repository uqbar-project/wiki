Definición en el Prelude
------------------------

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

Bonus: Asociatividad
--------------------

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
