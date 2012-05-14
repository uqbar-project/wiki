Tuplas y listas en Haskell
--------------------------

Una cosa que suele confundir a la hora de diferenciar listas y tuplas es la idea de que "el número de componentes de una tupla es fijo", ¿qué quiere decir que es fijo si yo puedo tener 2-uplas, 3-uplas... n-uplas?

Lo que quiere decir es que las tuplas de distinta [aridad](aridad.html) son de *distinto tipo*, es decir no son comparables entre sí. Es decir la tupla y la tupla son de tipos distintos y yo no puedo hacer una función que acepte ambas tuplas (bah, debería ser polimórfica, ya es otra cuestión.

En cambio, las listas y son ambas del mismo tipo () y entonces sí puedo hacer una función que funcione en ambos casos.

### Ejemplos básicos

Comencemos por ver qué tipos me dice el Haskell que tiene cada uno de los valores que mencionamos antes:

`Prelude> :t (True, True)`
`(True, True) :: (Bool, Bool)`
`Prelude> :t (True, True, True)`
`(True, True, True) :: (Bool, Bool, Bool)`
`Prelude> :t [True, True]`
`[True, True] :: [Bool]`
`Prelude> :t [True, True, True]`
`[True, True, True] :: [Bool]`

Lo que hay que mirar en el ejemplo anterior es que las dos tuplas tienen tipos distintos ( y ), mientras que las dos listas tienen el mismo tipo ().

Además podemos ver que el tipo de una tupla indica la cantidad de componentes, mientras que el tipo de una lista no... eso quiere decir que todas las listas de booleanos son del mismo tipo, independientemente de la cantidad de componentes.

### Errores de tipo

Es fácil ver que si intentamos trabajar con las dos tuplas como si fueran del mismo tipo vamos a tener problemas, un ejemplo sencillo es compararlas por igualdad. Si comparamos las dos listas obtenemos el resultado esperable:

`Prelude> [True,True] == [True,True,True]`
`False`

pero si intentamos comparar las dos tuplas ocurre un error:

`(False, 'H') :: (Bool, Char)`
`Prelude> (True,True) == (True,True,True)`
<interactive>`:1:15:`
``     Couldn't match expected type `(Bool, Bool)' ``
``            against inferred type `(Bool, Bool, Bool)' ``
``     In the second argument of `(==)', namely `(True, True, True)' ``
`    In the expression: (True, True) == (True, True, True)`
``     In the definition of `it': it = (True, True) == (True, True, True) ``

### Uso en funciones

También podemos ver que una función puede manejar listas de cualquier longitud pero no pasa lo mismo con las tuplas. Si intentamos hacer una función todosVerdaderos, con listas es fácil:

`todosVerdaderosL [] = True`
`todosVerdaderosL (x:xs) = x && todosVerdaderosL xs`

Al cargar eso en el Haskell obtenemos:

`Prelude> :l todosVerdaderos.hs`
`[1 of 1] Compiling Main             ( todosVerdaderos.hs, interpreted )`
`Ok, modules loaded: Main.`
`*Main> todosVerdaderosL [True,True]`
`True`
`*Main> todosVerdaderosL [True,True,True]`
`True`

Podríamos intentar hacer lo mismo con tuplas, para ello agrego las dos definiciones:

`todosVerdaderosT (x,y) = x && y`
`todosVerdaderosT (x,y,z) = x && y && z`

Pero lamentablemente eso no es posible en Haskell:

`*Main> :r`
`[1 of 1] Compiling Main             ( todosVerdaderos.hs, interpreted )`
`todosVerdaderos.hs:5:17:`
``     Couldn't match expected type `(Bool, Bool)' ``
``            against inferred type `(a, b, c)' ``
`    In the pattern: (x, y, z)`
``     In the definition of `todosVerdaderosT': ``
`        todosVerdaderosT (x, y, z) = x && y && z`
`Failed, modules loaded: none.`

Una forma de definir la función es:

`todosVerdaderosT2 (x,y) = x && y`
`todosVerdaderosT3 (x,y,z) = x && y && z`

eso si pasa los chequeos del haskell:

`Prelude> :r`
`[1 of 1] Compiling Main             ( todosVerdaderos.hs, interpreted )`
`Ok, modules loaded: Main.`

Pero vemos que me obliga a usar las funciones separadamente:

`*Main> todosVerdaderosT2 (True,True)`
`True`
`*Main> todosVerdaderosT3 (True,True,True)`
`True`

Finalmente, podemos ver que si se intenta intercambiar las funciones se produce un error:

`*Main> todosVerdaderosT2 (True,True,True)`
<interactive>`:1:18:`
``     Couldn't match expected type `(Bool, Bool)' ``
``            against inferred type `(Bool, Bool, Bool)' ``
``     In the first argument of `todosVerdaderosT2', namely ``
``         `(True, True, True)' ``
`    In the expression: todosVerdaderosT2 (True, True, True)`
``     In the definition of `it': ``
`        it = todosVerdaderosT2 (True, True, True)`

Para más información, ver acá: [Pattern\_Matching\_en\_Haskell](pattern-matching-en-haskell.html)

Functores y listas en prolog
----------------------------

TBC
