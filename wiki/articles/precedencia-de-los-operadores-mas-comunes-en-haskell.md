---
layout: article
title: Precedencia de los operadores mas comunes en haskell
---

Definición en el Prelude
------------------------

La siguiente tabla muestra la precedencia de los operadores que más utilizamos en Haskell. A mayor número mayor precedencia. Por ejemplo, el operador `+` tiene mayor precedencia que `<`, por lo tanto si escribimos:

```hs
3 < 4 + 5
```

se entiende como:

```hs
3 < (4+5)
```

La tabla (simplificada) es la siguiente:

```hs
-- Primero vienen las funciones prefijas (como even, map, etc.) y luego los chirimbolos:
infixr 9  .
infixl 9  !!
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`, :, %
infixl 6  +, -
infixr 5  :
infixr 5  ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||
infixr 0  $
```

Eso significa que el signo pesos (la función `$`) tiene **muy poca precedencia**, va a ser la "última" en considerarse. Y por el contrario las funciones prefijas (como el `even` o el `abs`) van a tener **mucha precedencia**. Entonces esta cuenta:

```hs
abs 3 - 5 -- Se lee como "le resto 5 al valor absoluto de 3"
```

Debe leerse como "le resto 5 al valor absoluto de 3", porque eso es lo que significa en Haskell, porque la aplicación prefija de `abs` tiene **más precedencia** que el `-`. El resultado de esa cuenta es `(-2)` (negativo).

Lo mismo sucede acá:
```hs
4 > 2 * 3 -- Se lee como "quiero saber si el 4 es mayor a la multiplicación entre 2 y 3"
```
Se lee así porque el `>` tiene **menos precedencia** que el `*`, entonces **el `*` se hace primero**

Por último, para **romper la precedencia** se usan paréntesis. Si yo quiero decir "el valor absoluto de la resta entre 3 y 5" lo que debo hacer es:

```hs
abs (3 - 5) -- Se lee como "el valor absoluto de la resta entre 3 y 5"
```

Uso del signo pesos ($) para evitar paréntesis
----------------------------------------------

Ver [Uso del signo pesos en Haskell](el-signo-pesos-en-haskell.html) para ver cómo se aprovecha que el `$` tenga tan poca precedencia.


Bonus: Asociatividad
--------------------

Las palabras clave `infixl` e `infixr` permiten indicar la *asociatividad* del operador. Los operadores definidos con `infixl` asocian a izquierda, mientras que los `infixr` asocian a derecha. Por lo tanto, la expresión:

```
3 + 4 + 5
```

Se evalúa como:

```
(3 + 4) + 5
```

Ya que el operador asocia a izquierda. En cambio la expresión:

```
2:3:4:[]
```

Se debe leer como:

```
2:(3:(4:[]))
```

Ya que el operador asocia a derecha, al igual que la composición (`.`), por ejemplo:

```
snd . head . filter even
```

Debe leerse como:

```
snd . (head . filter even)
```

También puede notarse que todos los operadores tienen menor precedencia que la aplicación funcional, es decir que al ejemplo anterior podríamos definirlo completamente si agregamos los paréntesis alrededor de `.`:

```
snd . (head . (filter even))
```

Los operadores definidos como `infix` no son asociativos, por ejemplo el operador de igualdad `==`. Por lo tanto la expresión:

```
a == b == c
```

No se entiende como `(a == b) == c` ni como `a == (b == c)`; la expresión sin paréntesis es incorrecta.
