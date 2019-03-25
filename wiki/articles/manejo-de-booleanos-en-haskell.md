---
layout: article
title: Manejo de booleanos en haskell
---

El Tipo Bool
------------

Los booleanos son datos que representan la condición de verdad o falsedad.

Recordemos que `even` es una función que recibe un número, y devuelve un booleano. (True si el número es par)

Hagamos estas consultas en la consola (El simbolito `>` al inicio de la línea indica que se está haciendo una consulta):

```haskell
> even 6 
True
> not True
False
> not False
True
```

Aquí se ve que **`not`** es una función recibe un Bool (y que devuelve el booleano opuesto).

Hay otras funciones que devuelven booleanos:

```haskell
> 5 < 1
True
> "mama" == "papa"
False
```

Entonces, si queremos que una función devuelva un booleano, basta con devolver el mismo resultado que obtuvimos. Por ejemplo,

```haskell
--"la golondrina pepita está empachada si su energía es mayor que 100"
estaEmpachada energia = energia > 100
```

Conjunción y Disjunción Lógica
------------------------------

```haskell
> True || False
True
> True && False
False
```

Para saber si el 6 es par y divisible por 3:

```Haskell
> even 6 && (esDivisible 6 3)
True
```

Errores Comunes
---------------

### "true" vs true vs True

En Haskell,

-   true es un nombre de variable ó de función. Si trato de usarlo, me va a tirar el error "Not in Scope".

```Haskell
> not true
``Not in scope: `true'``
```

-   "true" es un String, por lo que si se lo mando a una función que espera un booleano, va a haber un error de tipos:

```Haskell
> not "true"
`` Couldn't match expected type `Bool' ``
`` with actual type `[Char]' ``
```

-   True es la manera correcta de referirse al booleano. True es un [Constructor](constructor.html) del tipo de dato Bool.

```Haskell
> not True
False
```

### Problemas con Booleanos y Guardas

-   En [Funciones\_por\_Partes\#Errores\_Comunes](funciones-por-partes.html) hay algunos errores comunes.

#### Mal Uso de Booleanos (algo == True, etc.)

Una variante de los problemas con Guardas es lo siguiente:

```Haskell
desaprueba nota = estaAprobada nota == False
pierdePromocion nota = estaAprobada nota == True && nota < 8
```

Ese código está mal. ¿Por qué? Porque **estaAprobada ya devuelve un Booleano**. De la misma manera **la función menor también devuelve un Booleano**. Entonces, en el caso de pierdePromocion, está demás la comparación por True. Y en el caso del desaprobada, si lo que necesito es el opuesto del booleano, entonces debo usar la función not:

```Haskell
desaprueba nota = not (estaAprobada nota)
pierdePromocion nota = estaAprobada nota && nota < 8
```

Ese código es el correcto. Comprobamos así:

```Haskell
> estaAprobada 7 == True
True
> estaAprobada 7
True
> estaAprobada 3 == False
True
> not (estaAprobada 3)
True
```
