---
layout: article
title: Aplicacion
---

Cuando se aplica una función a unos argumentos, el resultado se obtiene sustituyendo esos argumentos en el cuerpo de la función respetando el nombre que se les dio a cada uno. Es posible que esto produzca un resultado que no puede ser simplificado (e.g. un número); pero es más común que el resultado de esa sustitución sea otra expresión que contenga otra aplicación de funciones, entonces se vuelve a hacer el mismo proceso hasta producir un resultado que no pueda ser simplificado (un resultado final).

Por ej., siendo doble:

`doble x = x + x`

El resultado de aplicar doble 3 (o sea, la función doble aplicada al número 3) se puede obtener de la siguiente manera:

`doble 3`
`= { aplicamos la función doble }`
`3 + 3`
`= { aplicamos la función + }`
`6`

Si queremos obtener el resultado de doble (doble 2) en donde la función doble es aplicada 2 veces, podemos hacer:

`doble (doble 2)`
`= { aplicamos la función doble que está dentro del paréntesis}`
`doble (2 + 2)`
`= { aplicamos la función + }`
`doble 4`
`= { aplicamos la función doble }`
`4 + 4`
`= { aplicamos la función + }`
`8`

También podemos obtener el mismo resultado aplicando primero la función doble de más afuera:

`doble (doble 2)`
`= { aplicamos la función doble que está afuera del paréntesis }`
`doble 2 + doble 2`
`= { aplicamos la primer función doble }`
`(2 + 2) + doble 2`
`= { aplicamos la primer función + }`
`4 + doble 2`
`= { aplicamos la función doble }`
`4 + (2 + 2)`
`= { aplicamos la segunda función + }`
`4 + 4`
`= { aplicamos la función + }`
`8`

El orden en que realicemos las **reducciones** (basta de decirle simplificar) no afecta al resultado final pero sí a la eficiencia. Lo copado es que hay un motor que se encarga de esto (se llama motor de reducciones CUAC!). Para entender un poco más lo que pasa detrás de escenas hace falta meterse en la [estrategia de evaluación](estrategias-de-evaluacion.html) usada por el motor.

Aplicación de funciones en Haskell
----------------------------------

Notacion matemática

`f(a, b) + c d`

En Haskell

`f a b + c * d`

La aplicación se denota solo poniendo espacio entre la función y sus argumentos. Además la aplicación de funciones que no son operadores tiene una precedencia mayor que la de los operadores (un operador es una función que recibe 2 parámetros y se usa de forma infija ya sea un chirimbolo o una función con \`\`) � Las siguientes definiciones son equivalentes, podemos ver la precedencia para la aplicación de div de forma infija y prefija:

`promedio ns = (sum ns) ‘div‘ (length ns)`
`promedio' ns = sum ns ‘div‘ length ns`
`promedio'' ns = div (sum ns) (length ns)`
