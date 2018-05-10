---
layout: article
title: Calculo del tipo de una funcion en haskell
categories: [haskell]
---

# Introduccion

Dado que el haskell es un lenguaje con [Inferencia de tipos](inferencia-de-tipos.html), no es necesario indicar el tipo de las funciones que construimos. A pesar de ello (o tal vez precisamente *por* ello) los tipos juegan un rol fundamental al programar en un lenguaje funcional (en particular en Haskell).

El sistema de tipos de Haskell es muy estricto, bastante más de lo que estamos acostumbrados los que venimos de la programación orientada a objetos. Si bien en algunos casos puede ser molesto porque un tipado tan estricto como el de Haskell complica algunas operaciones sencillas como suele pasar con el [cálculo de un promedio](problemas-comunes-con-los-tipos-numericos-de-haskell.html), en la amplia mayoría de los casos es bueno porque nos ayuda a detectar errores más tempranamente. 

Esto obliga al programador a ser más atento con los tipos de cada una de las funciones que programa o que utiliza. Los siguientes aspectos de tipado son importantes a tener en cuenta a la hora de entender de qué tipo son las funciones que definimos:

## La construcción de funciones a partir de funciones  
El paradigma funcional tiene como uno de sus pilares la posibilidad de construir funciones complejas a partir de combinar funciones más simples (utilizando composición, aplicación parcial, orden superior, etc); para poder utilizar cualquiera de esas herramientas es necesario tener presente el tipo de las funciones que quiero combinar, por ejemplo:

- Si quiero componer dos funciones `f` y `g`: es necesario que la imagen de `f` esté incluida en el dominio de `g` (tal como aprendimos en matemática).
- Al aplicar una función de orden superior es necesario que *matcheen* los tipos de la función esperada con la función recibida.

## El polimorfismo paramétrico y las type classes  
Una función que tiene un tipo genérico al ser aplicada puede reducir su tipo, eso también es algo a tener en cuenta.

Por ejemplo la función `filter` puede en principio procesar listas de cualquier tipo; en cambio una vez que yo le aplico el primer parámetro (el criterio de selección) ese tipo se restringe. Si el criterio fuera `even`, ese filtrado sólo va a servir para listas de valores numéricos, porque `even` restringe el tipo de los elementos de la lista a el tipo de lo que espera recibir, que es un número.

A continuación se describen paso a paso los ejemplos que permiten comprender el mecanismo de inferencia utilizado en el lenguaje Haskell.

# Ahora sí: ¿cómo inferimos el tipo de una función?

Antes de poder evaluar el tipo de una hay que comprender cuáles son los tipos posibles de Haskell, eso está explicado en el artículo sobre [Tipos de Haskell](tipos-de-haskell.html), y entender la regla básica de tipado para una aplicación: si `x` es Bool, entonces `not x` también es de tipo Bool, porque `not :: Bool -> Bool`; si `x` es de cualquier otro tipo `not x` no tipa.

## Funciones Simples

Al intentar calcular el tipo de una función, lo primero que tenemos que hacer es mirar las funciones que se usan dentro de su definición y asegurarnos de saber de qué tipo son esas funciones. Luego las preguntas importantes son:
1. ¿Cuántos parámetros recibe? Esto ayuda a ordenarnos, para saber cuántos huecos tenemos que llenar, que es esa cantidad + 1, por el tipo de retorno.
2. ¿De qué tipo son esos parámetros? Esto se deduce en base al uso de los mismos en la definición.
3. ¿De qué tipo es lo que retorna? Esto se deduce en base a lo que retorna la función principal que es la "de más afuera" o menor precedencia.

Por ejemplo:

```haskell
none x y = not x && not y

-- Sabemos que:
-- not :: Bool -> Bool
-- (&&) :: Bool -> Bool -> Bool
```

Luego, para determinar el tipo de la función `none` podemos seguir los siguientes pasos:

1. Vemos que tiene dos parámetros (`x` e `y`), entonces podemos decir que su tipo tiene que tener la forma `none :: ?? -> ?? -> ??`, luego tendremos que calcular cuáles son esas incógnitas.
2. Analizamos el tipo de los parámetros:
  - Si `x` es utilizado como parámetro de la función `not`, podemos deducir que `x` no admite valores de cualquier tipo, sólo pueden ser booleanos. Por ende: `none :: Bool -> ?? -> ??`.
  - Un razonamiento análogo nos lleva a deducir que `y` también debe ser un valor booleano. Luego: `none :: Bool -> Bool -> ??`.
3. Finalmente, para saber el tipo de retorno:
  - Dado que `x` es Bool, entonces `not x` también es de tipo Bool, al igual que `not y`, lo cual es compatible con lo que espera el `(&&)` (o sea que la expresión `not x && not y` tipa).
  - Lo que retorna la función `(&&)` al estar totalmente aplicada es Bool, y esa es la función principal, así que podemos afirmar que: `none :: Bool -> Bool -> Bool`.

En el último paso podemos ver que en realidad para saber el tipo de no sería necesario mirar los parámetros de `(&&)`, con saber su tipo de retorno sería suficiente. Sin embargo el análisis es útil para asegurarnos de que la función es correcta, y en caso de incurrir en errores de tipos, entender la causa.

Por otro lado, en ejemplos más complejos analizar los parámetros de las funciones usadas en la definición será indispensable para poder saber el tipo de retorno (por ejemplo en la presencia de polimorfismo).

## Ejemplo un poco mas heavy

Siendo

```haskell
f x y z = (head y) > (map (\n -> n x) z)
```

Vamos a intentar hacer la inferencia de tipos. Primero tenemos que ver qué es `f`? f es una función que tiene 3 parámetros

Ponemos 3 flechitas simples: ->

```
f :: esto es el tipo de x -> esto es el tipo de y -> esto es el tipo de z -> esto es el tipo de lo que devuelve f
```

Como `head :: [a] -> a`, `y` tiene que ser una lista

```
f :: esto es el tipo de x -> [???] -> esto es el tipo de z -> esto es el tipo de lo que devuelve f
```

Como `map :: (a -> b) -> [a] -> [b]`, `z` tiene q ser una lista porque se usa como su segundo parámetro

```
f :: esto es el tipo de x -> [???] -> [???] -> esto es el tipo de lo que devuelve f
```

La función `(\n -> n x)` que es primer parámetro del `map` recibe como parámetro cada elemento de la lista `z`, cada uno de esos elementos va a ser `n`. Y como `n` se está aplicando a `x` podemos inferir que `n` es una función, por lo que `z` es una lista de funciones

```
f :: esto es el tipo de x -> [???] -> [??? -> ???] -> esto es el tipo de lo que devuelve f
```

Como `x` es el parámetro de `n` podemos inferir que `x` pertenece al dominio de `n`, por ende si el su dominio es de tipo **a** entonces `x` es de tipo **a**

```
f :: a -> [???] -> [a -> ???] -> esto es el tipo de lo que devuelve f
```

Respiremos profundo... Asumimos que la imagen de las funciones de la lista es de tipo **b**, porque sólo a partir de `map (\n -> n x) z` no vemos nada que lo restrinja a tipos concretos, ni que deba ser del mismo tipo que su dominio al cual denominamos **a**

```
f :: a -> [???] -> [a -> b] -> esto es el tipo de lo que devuelve f
```

Ahora pensemos en los parámetros de la función `(>) :: Ord a => a -> a -> a` que son `(head y)` y `(map (\n -> n x) z)`:
- Para poder comparar estas 2 cosas, ambas expresiones tienen que ser del mismo tipo
- El `map` me da una lista de lo que devuelve `(\n -> n x)` sabemos que la imagen de `(\n -> n x)` es b entonces `map (\n -> n x) z` es de tipo **\[b\]**
- Por ende `(head y)` también es de tipo **\[b\]**
- Para que `(head y)` sea de tipo **\[b\]**, `y` tiene que tener el tipo **\[\[b\]\]**
- A su vez **b** debe pertenecer a la typeclass Ord (por la restricción impuesta por la función `(>)`)

```
f :: Ord b => a -> [[b]] -> [a -> b] -> esto es el tipo de lo que devuelve f
```

La función principal de `f` es `(>)`, como la imagen de `(>)` al estar totalmente aplicado es **Bool** la imagen de `f` es **Bool**

```
f :: Ord b => a -> [[ b ]] -> [a -> b] -> Bool
```

## Ejemplo de parcial para pensar

Tenemos esta función:


```haskell
f a b c d = maximoSegun (c d).filter (== snd a).map b
```

Y sabemos que:

```haskell
*Main> :t maximoSegun
maximoSegun :: Ord a1 => (a -> a1) -> [a] -> a
```

Cuál es el tipo de f? Ayudita: pensar cuál es la función principal en este ejemplo. Si no sabés bien qué está pasando, te recomendamos leer sobre [notación point-free](notacion-point-free.html).
