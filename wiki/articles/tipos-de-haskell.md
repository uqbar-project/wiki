---
layout: article
title: Tipos de haskell
---

Tal vez convenga aclarar que esta no es una categorización teórica, es más bien una introducción y vamos de lo más simple a lo más complejo.

## Tipos Simples

Los tipos más básicos que tenemos en Haskell son los booleanos y los caracteres, que tiene los tipos y respectivamente:

```Haskell
Prelude> :t True
True :: Bool
Prelude> :t 'a'
'a' :: Char
```

Qué podemos hacer con ellos? Algunos ejemplos con booleanos y caracteres:

```Haskell
Prelude> True && False
False
Prelude> True || False
True
Prelude> not True
False

Prelude> Char.isLower 'a'
True
Prelude> Char.isUpper 'a'
False
Prelude> Char.toLower 'A'
'a'
Prelude> Char.toUpper 'a'
'A'
Prelude> 'b' > 'a'
True
```

El tipo de los números es un poquito más complejo porque tenemos números enteros, reales, racionales y muchas más variantes, lo vamos a ver en más detalle más adelante. Algunas operaciones que podemos hacer:

```Haskell
Prelude> 4*6
24
Prelude> 2+3
5
Prelude> 9-4
5
Prelude> 4 `div` 3 
1
Prelude> 4 `mod` 3 
1
Prelude> 4/6
0.6666666666666666
```

La división en ocasiones puede traer problemas de tipos, si tuviste problemas con esto leé el [siguiente artículo](problemas-comunes-con-los-tipos-numericos-de-haskell.html)

## Tipos Compuestos

### Listas y Strings

Un String es simplemente una lista de caracteres que puede escribirse con una sintaxis particular, los siguientes ejemplos son equivalentes:

```Haskell
Prelude> :t "Hola"
"Hola" :: [Char]
Prelude> :t ['H','o','l','a']
['H','o','l','a'] :: [Char]
```

Para cualquier lista el tipo se escribe poniendo entre corchetes el tipo de los elementos la lista, por ejemplo una lista de booleanos:

```Haskell
Prelude> :t [True, False]
[True, False] :: [Bool]
```

También una lista de Strings (o lista de listas de caracteres):

```Haskell
Prelude> :t ["Hola", "Chau"]
["Hola", "Chau"] :: [ [Char] ]
```

### Tuplas

Una tupla es también un valor compuesto. A diferencia de las listas el número de componentes es fijo y los componentes pueden ser cada uno de un tipo distinto. (Esto está más detallado en [¿Cuál es la diferencia entre una tupla y una lista?](cual-es-la-diferencia-entre-una-tupla-y-una-lista-.html), pero antes te recomiendo mirar los ejemplos que siguen.)

Una de las tuplas más simples que se puede imaginar es:

```Haskell
Prelude> :t (True, 'H')
(True, 'H') :: (Bool, Char)
```

es decir, una tupla compuesta por un booleano y un caracter. Sin embargo, los elementos de las tuplas también pueden ser compuestos, como un String:

```Haskell
Prelude> :t (True, "Hola")
(True, "Hola") :: (Bool, [Char])
```

o inclusive el componente de una tupla puede ser otra tupla:

```Haskell
Prelude> :t (False, ('H', "ola"))
(False, ('H', "ola")) :: (Bool, (Char, [Char]))
```

También podemos tener tuplas de más de dos componentes

```Haskell
Prelude> :t (True, 'H', [False])
(True, 'H', [False]) :: (Bool, Char, [Bool])
```

es decir, un booleano, un caracter y una lista de booleanos.

Para finalizar podemos ver un ejemplo en el que combianmos todo lo anterior (no es trivial, tomate tu tiempo para leerlo!):

```Haskell
Prelude> :t ([True, False, True], "Chau", [(True, "True"), (False, "False")])
([True, False, True], "Chau", [(True, "True"), (False, "False")])
  :: ([Bool], [Char], [(Bool, [Char])])
```

Es decir, una tupla de tres componentes, a saber:

-   Una lista de booleanos
-   Un string o lista de caracteres
-   Una lista de tuplas cuyo primer componente es un booleano y su segundo componente es un string.

### Type

Haskell nos permite definir sinónimos de tipos, o sea definir un alias para un tipo concreto que se use para modelar alguna abstracción que nos interese.

Por ejemplo si modelamos a los alumnos como tuplas de aridad 2 cuyo primer elemento es el nombre y el segundo una lista de números que representa las notas que se sacó, podríamos definir un tipo Alumno como:

```Haskell
type Alumno = (String, [Int])
```

De hecho, String no es más que un alias para el tipo \[Char\]. Sin embargo, es importante tener en cuenta que estos sinónimos sólo son útiles a efectos de declarar los tipos de otros datos o de las funciones de nuestro programa, pero para el motor de inferencia de tipos, estos alias son ignorados con lo cual si preguntamos de qué tipo es una función en donde se espera un alumno por parámetro, nos dirá (String, [Int]) y no Alumno.

### Data

Es posible [definir nuestros propios tipos de dato](data--definiendo-nuestros-tipos-en-haskell.html) usando data para poder mejorar las abstracciones de nuestros programas y evitar algunos problemas asociados al uso de tuplas. Esto puede hacerse de la siguiente forma:

```Haskell
data NuevoTipo = Constructor Tipo1 Tipo2 ... TipoN
```

Usamos el constructor, como su nombre lo indica, para construir nuestros datos de este tipo y para hacer pattern matching como hacíamos con las tuplas.

```Haskell
data Alumno = UnAlumno String [Int]
fede = UnAlumno "Federico" [2,3]
nombreAlumno (UnAlumno nombre notas) = nombre
> :t nombreAlumno
nombreAlumno :: Alumno -> String
> nombreAlumno fede
"Federico"
```

El nombre del constructor puede coincidir con el nombre del tipo de dato, en este caso usamos nombres distintos para denotar que son cosas distintas y en qué contexto se usa cada una.

## Funciones

### Funciones con un único parámetro

El tipo de una función que tiene un parámetro se indica relacionando mediante el símbolo la entrada o dominio de la función con la salida o imagen. Por ejemplo la función not recibe un booleano y devuelve otro:

```Haskell
Prelude> :t not
not :: Bool -> Bool
```

La función isLower recibe un caracter y devuelve un booleano.

```Haskell
Prelude> :t isLower 
isLower :: Char -> Bool
```

(Nótese que la función isLower está en el módulo Char, dependiendo de su versión de Haskell tal vez deban escribir para poder probar el ejemplo, o bien importar el módulo correspondiente.)

Y la función and recibe una lista de booleanos y devuelve un booleano (resultado de realizar la conjunción entre todos los booleanos de la lista)

```Haskell
Prelude> :t and
and :: [Bool] -> Bool
```

### Funciones con más de un parámetro

Las funciones de más de un parámetro tienen alguna sutileza porque en Haskell se trabaja con el concepto de [Currificación](currificacion.html), entonces una función que nosotros en matemática estaríamos acostumbrados a verla como en Haskell la vamos a escribir . Las funciones de dos parámetros cuyo tipo tiene esa forma se denominan *currificadas*.

(A los efectos de entender el sistema de tipos podemos pensarlo simplemente como una función que recibe dos booleanos, aunque en realidad la versión currificada es mucho más poderosa. Para más detalles ver la teoría sobre [Currificación](currificacion.html).)

El tipo que usamos como ejemplo en el párrafo anterior corresponde (entre otros) a la función

```Haskell
Prelude> :t (&&)
(&&) :: Bool -> Bool -> Bool
```

### Aplicación

La [aplicación](aplicacion.html) es uno de los temas que tal vez más confunden cuando se habla de tipos de datos. La confusión más frecuente radica en no diferenciar correctamente *una expresión que tiene valor Booleano* de *una función que devuelve Booleanos*.

Ya vimos dos ejemplos de funciones que devuelven booleanos, con uno y dos parámetros:

```Haskell
Prelude> :t not
not :: Bool -> Bool
Prelude> :t Char.isLower
Char.isLower :: Char -> Bool
Prelude> :t and
and :: [Bool] -> Bool
Prelude> :t (&&)
(&&) :: Bool -> Bool -> Bool
```

En este punto es importante entender que ninguno de estos ejemplos es un *valor booleano*. Cuando veo el tipo eso se entiende como el tipo *de las funciones a las que si les aplico un parámetro de tipo Char producen un valor de tipo Bool*, que claramente no es lo mismo que el tipo Bool.

Lo dicho, si le aplicamos los parámetros adecuados a esas funciones, podemos obtener valores booleanos:

```Haskell
*Main> :t not True
not True :: Bool
*Main> :t Char.isLower 'a'
Char.isLower 'a' :: Bool
*Main> :t and [True, False, True]
and [True, False, True] :: Bool
*Main> :t True && False
True && False :: Bool
```

En síntesis es un valor booleano, en cambio no es un valor booleano, es una función que devuelve booleanos. También es *un valor*, pero es un valor de otro tipo y no se pueden mezclar.

Si intentamos utilizar un valor función en un lugar donde se espera un valor booleano, obtendremos un error:

```Haskell
*Main> not Char.isLower
<interactive>:1:4:
     Couldn't match expected type `Bool' 
            against inferred type `Char -> Bool' 
     In the first argument of `not' ... 
```

Es decir, el primer argumento de debe ser y en cambio se recibió un argumento de tipo .

Similarmente:

```Haskell
*Main> True && not
<interactive>:1:8:
     Couldn't match expected type `Bool' 
            against inferred type `Bool -> Bool' 
    In the second argument of `(&&)', namely `not' 
    In the expression: True && not
    ....
```

## Typeclasses

Si en algún momento consultaste el tipo de una función y viste algo como: `Num a => ...`, `Ord a => ...` o `Eq a => ...`, significa que esa función puede trabajar con distintos tipos concretos (como ser Int o Float). Num, Ord y Eq (al igual que otras menos cotidianas) no son tipos, sino familias de tipos que imponen restricciones sobre qué valores pueden usarse para evaluar esa función. A las familias de tipos les vamos a llamar [Typeclasses](typeclasses.html).

