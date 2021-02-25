---
layout: article
title: Precedencia de los operadores mas comunes en haskell
---

Definición en el Prelude
------------------------

La siguiente tabla muestra la precedencia de los operadores que más utilizamos en Haskell. A mayor número mayor precedencia. Por ejemplo, el operador `+` tiene mayor precedencia que `<`, por lo tanto si escribimos:

`3 < 4 + 5`

se entiende como:

`3 < (4+5)`

La tabla (simplificada) es la siguiente:

```
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
snd . (head . filter even).
```

También puede notarse que todos los operadores tienen menor precedencia que la aplicación funcional, es decir que al ejemplo anterior podríamos definirlo completamente si agregamos los paréntesis alrededor de `.`:

```
snd . (head . (filter even)).
```

Los operadores definidos como `infix` no son asociativos, por ejemplo el operador de igualdad `==`. Por lo tanto la expresión:

```
a == b == c
```

No se entiende como `(a == b) == c` ni como `a == (b == c)`; la expresión sin paréntesis es incorrecta.
