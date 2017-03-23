---
layout: article
title: Pattern matching en haskell
---

Pattern Matching es el concepto asociado al chequeo estructural de un dato respecto de una estructura esperada. El uso de pattern matching tiene la ventaja de simplificar mucho la codificación, ya que sólo escribimos la forma de lo que esperamos y podemos desglosar los componentes de estructuras complejas.

Una desventaja asociada a usar pattern matching en vez de funciones para acceder a los elementos de las tuplas es que nuestro código se verá muy afectado ante un cambio en las estructuras manejadas. El simple hecho de agregar un elemento más a la tupla que representa a nuestro elemento de dominio provocará un cambio en el tipo del mismo y el mismo se propagará en todos los lugares donde se haya utilizado esta estructura directamente.

Ejemplos de matcheo
-------------------

### Con números y strings

Si queremos saber si el nombre de un día de la semana es fin de semana podemos hacer:

```haskell
esFinDeSemana dia = dia == "Sábado" || dia == "Domingo"
```

Sin embargo esta otra definición también es válida:

```haskell
esFinDeSemana' "Sábado" = True
esFinDeSemana' "Domingo" = True
esFinDeSemana' _ = False
```

La última definición es necesaria, ya que a diferencia del paradigma lógico, no contamos con el [ principio de universo cerrado](paradigma-logico---introduccion-universo-cerrado.html) y si sólo definimos esFinDeSemana' para "Sábado" y "Domingo" y consultamos por otro día, Haskell nos mostrará un error porque el patrón no calza con ningún elemento del dominio esperado. Para que esta función funcione correctamente es importante que el encabezado con la [ variable anónima](pattern-matching-en-haskell-sobre-la-variable-anonima.html), que matchea con cualquier patrón, sea la última definición de esFinDeSemana', de lo contrario no habrá ningún elemento del dominio cuya imagen pueda ser True (por unicidad).

Acá tenemos un ejemplito [recursivo](recursividad-en-haskell.html) típico para pattern matching con números:

```haskell
factorial 0 = 1 
factorial n = n * factorial (n - 1)
```

Para evitar loops infinitos, es importante poner el caso base primero para que matchee con el 0, ya que la variable n también matchearía con este valor.

### Con tuplas

Si tengo los valores

```haskell
v1 = (2,5)
v2 = (3,5)
v3 = (8,5)
```

El patrón `(x,5)` matchea con los tres valores.

También el patrón `(x,y)` matchea con los tres valores.

También el patrón `(_,5)` matchea con los tres valores.

También el patrón `(_,_)` matchea con los tres valores.

También el patrón `unaTupla` matchea con los tres valores.

### Con listas

Ahora, si tengo una lista

```haskell
lista1 = [(2,5),(5,3),(3,3)]
lista2 = [(2,5)]
```

El patrón `(x,5)` no matchea con `lista1` ni con `lista2`. ¡Es una tupla, no una lista! Antes de tratar de matchear, Haskell nos va a tirar un error de tipos.

El patrón `(x:xs)` matchea con `lista1`, siendo `x = (2,5)` y `xs = [(5,3),(3,3)]` y matchea con `lista2`, siendo `x = (2,5)` y `xs = []`

El patrón `[x]` no matchea con `lista1` porque tiene más de un elemento, pero sí matchea con `lista2`, siendo `x = (2,5)`

El patrón `(x:_)` matchea con `lista1`, siendo `x = (2,5)` y matchea con `lista2`, siendo `x = (2,5)`

El patrón `(x:y:_)` matchea con `lista1`, siendo `x = (2,5)` e `y = (5,3)`, y no matchea con `lista2` porque sólo tiene un elemento

El patrón `unaTupla` matchea con `lista1` siendo `unaTupla = [(2,5),(5,3),(3,3)]` y matchea con `lista2` siendo `unaTupla = [(2,5)]`. Es sólo un nombre de variable, con lo cual no restringe realmente el tipo del valor para que sea una tupla.

### Con data

(En algunos cursos no se da data, para más información ver acá: [Data: Definiendo nuestros tipos en Haskell](data--definiendo-nuestros-tipos-en-haskell.html)) Suponiendo que tenés

```haskell
data Coordenada = Coord Int Int
```

(el tipo es Coordenada y el constructor es Coord)

Donde se puede aplicar la misma idea

```haskell
lista1 = [Coord 2 5,Coord 5 3,Coord 3 3]
lista2 = [Coord 2 5]
```

El patrón `(Coord x 5)` no matchea con `lista1` ni con `lista2`. ¡Es una Coordenada, no una lista! Antes de tratar de matchear, Haskell nos va a tirar un error de tipo.

El patrón `(Coord x y:restoCoords)` matchea con `lista1`, siendo `x = 2`, `y = 5` y `restoCoords = [Coord 5 3,Coord 3 3]` y matchea con `lista2`, siendo `x = 2`, `y = 5` y `restoCoords = []`

El patrón `[x]` no matchea con `lista1` pero si matchea con `lista2`, siendo `x = Coord 2 5`

El patrón `(x:_)` matchea con `lista1` y matchea con `lista2` siendo `x = Coord 2 5`

El patrón `(x:y:_)` matchea con `lista1` siendo `x = Coord 2 5` y `y = Coord 5 3` pero no matchea con `lista2`

El patrón `unaTupla` matchea con `lista1` y matchea con `lista2` porque no es más que una variable sin ninguna restricción después de todo

Patrones con sinónimos (**at** Pattern)
---------------------------------------

Usamos este patrón para definir un sinónimo, o sea necesito por un lado el patrón para tomar algunos de sus componentes y por otro todo junto para la hacer otra cosa, pongamos un par de ejemplos:

```haskell
ordenada [_] = True
ordenada (x1:x2:xs) = x1 > x2 && ordenada x2:xs
```

Puede mejorarse sutilmente con un sinónimo.

```haskell
ordenada [_] = True
ordenada (x1:xs@(x2:_)) = x1 > x2 && ordenada xs
```

Otro ejemplo, entre dos complejos representados con tuplas, obtener el que tiene mayor parte real:

```haskell
mayorParteReal (a@(a1, _)) (b@(b1, _))
 | a1 > b1 = a`
 | otherwise = b
```

Y otro con listas por comprensión:

```haskell
promedioDeAprobados alumnos = promedio [ promedio notas | alumno@(_,notas) <- alumnos, aprobo alumno ]
```

Sobre la Variable Anónima
-------------------------

La variable anónima \_ (guión bajo) funciona, en términos de matcheo, como una variable común:

```haskell
esCero 0 = True
esCero x = False
```

Es lo mismo que decir:

```haskell
esCero 0 = True
esCero _ = False
```

De hecho, esta última opción es mejor, porque ayuda a la expresividad: si la variable no se usa en la devolución (a la derecha del igual, ó a la derecha de la guarda) entonces mejor ni ponerle nombre. Por ejemplo:

```haskell
anioActual = 2014
edad (nombre, anioNac, cantPerros) = anioActual - anioNac
```

En ese caso la función edad recibe una tupla pero a la derecha, en la definición de la función, no hace faltan el nombre y el cantPerros. Entonces, usamos variables anónimas:

```haskell
edad ( _, anioNac, _) = anioActual - anioNac
```

### Error común con Variables Anónimas

Muchas veces pensamos que no necesitamos una variable, cuando en realidad sí la usamos a la derecha del igual. Por ejemplo, si queremos tomar esa definición de "persona" como una tupla de tres, y agregarle perros a la persona, debemos devolver a una persona, pero cambiando sólo la cantidad de perros.

Esto es **erróneo**:

```haskell
agregarPerros cant (_, _, cantPerros) = (_, _, cantPerros + cant)
```

¡Son variables anónimas! Si quiero usarlas a la derecha del igual (ó sea, si quiero que sean parte de lo que devuelvo) entonces tengo que usar variables comunes.

Esto está **bien**:

```haskell
agregarPerros cant (nombre, anioNac, cantPerros) = (nombre, anioNac, cantPerros + 1)
```
