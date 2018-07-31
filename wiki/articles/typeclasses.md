---
layout: article
title: Typeclasses
---

# Introducción

Problema
--------

Si tenemos la multiplicacion (\*) definida para tanto Int como Float, luego

`square x = x*x`

debería transformarse en tiempo de compilación en dos funciones distintas

square (para ints) square (para floats)

Lo cual crece exponencialmente, por ejemplo con funciones como

`squares (x,y,z) = (x*x, y*y, z*z)`

que sería traducida en 8 funciones distintas, de modo que se pueda usar tanto con una tupla de tipo (Int, Float, Int) como (Float, Float, Float) o (Int, Int, Float)...

Typeclasses al rescate
----------------------

Las typeclasses son un contrato o tipo de datos abstracto que agrupa las funciones sobrecargadas: es decir, definen una lista de funciones que un tipo deber implementar para considerarse de esa clase. Ejemplo: la typeclass Num dice que todos aquellos tipos que sean Num (Int o Float por ejemplo) van a definir las funciones (+) y (\*) y de que tipos son.

Las typeclasses tienen como primer consecuencia que funciones como squares, que tenía 8 tipos posibles, tenga uno solo:

`squares :: Num a, Num b, Num c => (a,b,c) -> (a,b,c)`

Donde Num es un typeclass que indica que a es un tipo concreto (como Int o Float) que cumple con ese contrato, y lo mismo para b y c.

# Las typeclasses de Haskell más usadas

## Num, Ord y Eq

Nos gustaría poder definir los siguientes tipos (a.k.a dominios e imágenes)

(+) :: (Si asumimos que a es **numérico**) entonces a -> a -> a

(>) :: (Si asumimos que a es **ordenable**) entonces a -> a -> Bool

(==) :: (Si asumimos que a es **equiparable**) entonces a -> a -> Bool

En Haskell eso se escribe de la siguiente manera

```Haskell
(+) :: (Num a) => a - > a -> a
(>) :: (Ord a) => a - > a -> Bool
(==) :: (Eq a) => a - > a -> Bool
```

Num, Ord y Eq son **restricciones de tipo**, en Haskell se las conoce como Typeclasses (no confundan esto con el término Clase que usamos en el paradigma de objetos!!)

En cada Typeclass se definen un conjunto de funciones que los tipos pertenecientes deben implementar. A continuación mostramos algunos ejemplo de funciones que se definen en Num, Ord y Eq.

```Haskell
Num a:
(+), (-), (*) :: a -> a -> a
negate, abs, signum :: a -> a
etc.

Ord a:
(<), (<=), (>=), (>) :: a -> a -> Bool
max, min :: a -> a -> a
etc.

Eq a:
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
```

Veamos cómo impacta esto en la definición de algunas funciones de listas por ejemplo:

```Haskell
elem :: (Eq a) => a -> [ a ] -> Bool
elem unElemento unaLista = any (unElemento==) unaLista

maximum :: (Ord a) => [a] -> a
maximum lista = foldl1 max lista
```

Lo que nos interesa más que nada es que se note que gracias a que existen las typeclasses que explicitan cuál es el conjunto mínimo de funciones que el tipo concreto debe definir para formar parte de ella, luego podemos construir otras cosas encima usando esas funciones y vamos a poder sacarle el mismo provecho. Las funciones `elem` y `maximum` van a funcionar tanto para \[Int\] como para \[Char\] (o sea, para strings).

### ¿Qué tipos pertenecen a cada restricción?

Num: Los tipos concretos más comunes que integran esta familia son Int y Float.

Ord: Además de incluír a la mayoría de los tipos numéricos (como Int y Float) incluye a los caracteres, los booleanos (sí, hay un orden preestablecido para los booleanos aunque no sea común usarlo). También incluye a las listas y las tuplas siempre y cuando los tipos que las compongan sean ordenables (por ejemplo los Strings, que son listas de caracteres, son ordenables). Las funciones **no son ordenables**.

Eq: Todos los Ord y los Num son también esquiparables. Para las listas y tuplas, son equiparables si los tipos que las componen lo son. Las funciones **no son equiparables**.

¿Y los data? Algo muy común es que querramos que nuestros tipos de datos sean equiparables. Si los tipos de datos que componen a nuestros data lo son, alcanza con derivar Eq y no necesitaremos definir la igualdad. De lo contrario, para que sea Eq será necesario dar un pasito más avanzado y declarar que nuestro tipo de dato es instancia de la typeclass Eq e incluir una definición para la función (==) como se explica en el artículo sobre [data](data--definiendo-nuestros-tipos-en-haskell.html).

También es muy común querer que nuestros datos se puedan mostrar, y para eso existe otra typeclass que es

## La restricción Show

Cuando utilizamos el intérprete, los resultados de nuestras funciones son valores (valores simples, compuestos o funciones), para que se puedan mostrar por pantalla esos valores tienen que tener una representación en forma de cadena de caracteres (String).

La magia la realiza una función llamada show

```Haskell
> show 3
"3"
> show True
"True"
> show (2,3)
"(2,3)"
```

Ahora bien, no todos los valores pueden ser parámetro de la función show. Por ejemplo las funciones no tienen una representación en String

```Haskell
> show length
Error
```

Debido a esto, cuando quieren mostrar por pantalla una lista de funciones (ej \[fst,snd\]), una función parcialmente aplicada como (3+), o en general una función que retorna una función les va a tirar un error.

Veamos el tipo de la función show

```Haskell
show :: (Show a) => a -> String
```

La única función que se define en la restricción Show es la función show

Qué tipos pertenecen a la restricción Show? Bool, Char, Double, Float, Int, Integer, (Show a) => \[ a \] --Listas, (Show a,b) => (a,b) --Tuplas

Resumiendo: casi todos todos los tipos menos el tipo función

Al igual que se mencionó anteriormente para Eq, podemos hacer que un tipo de dato propio pueda ser mostrado con la función Show ya sea usando **deriving** si sólo se compone de otros datos de tipo Show o haciendo que sea instancia de Show y definiendo la función Show como querramos.

# Para cerrar: El truco detrás de la magia

Las typeclasses se pueden traducir/reescribir a un lenguaje sin typeclasses. Y por lo tanto, como pueden reutilizarse los mecanismos de inferencia de tipos de uno para el otro.

Para esta traducción, se usará la metáfora de un method dictionary. Cada tipo va a tener un diccionario con sus funciones sobrecargadas. Luego, cada función con *polimorfismo paramétrico*, recibe como parámetro el method dictionary y usa la función adecuada:

Ejemplo:

`square x = x * x`

se transforma en algo como

```Haskell
--ambos diccionarios son del mismo tipo (que es el tipo definido en la typeclass ;) )
dictInt = ...
dictFloat = ....

multi dict = dict !! 1 -- suponiendo que la función multiplicación esta en el diccionario en la posición 1
square dict x = multi dict x x
```

Y cuando ejecutamos

`square 3`

se traduce, como 3 es un Int, en algo como

`square dictInt 3 `

Alguien que ya tiene conocimientos sobre el [paradigma orientado a objetos](paradigma-de-objetos.html) podría encontrar similitudes con el polimorfismo de ese paradigma desde el punto de vista del uso, pero la realidad es que hay magia en el medio a nivel compilador de modo que siempre se sabe qué definición de función se usará en tiempo de compilación (no se hace un dispatch dinámico).
