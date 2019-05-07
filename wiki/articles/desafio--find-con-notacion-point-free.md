---
layout: article
title: Desafio  find con notacion point free
---

Se desea definir la función find, que dado un criterio y una lista encuentra al primero que lo cumple:

```Haskell
Main> find even [1,35,36,9]
36
```

El objetivo es definirla así:

```Haskell
find = .........
```

O sea,

-   con notación point-free para ambos parámetros,
-   sin usar expresiones lambda ni funciones auxiliares.

Como la solución puede obtenerse probando un poco, para que la respuesta al desafío sea aceptada, debe ir acompañada de una pequeña explicación de cómo funciona.
