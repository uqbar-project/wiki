Que hace la siguiente función?

`autoAplicar funcion = funcion funcion`

OK, y que tipo tiene?

`autoAplicar :: (a->a) -> (a->a) ??`

Si lo cargamos

`Occurs check: cannot construct the infinite type: a = a -> a`
`` When generalising the type(s) for `f' ``

A que intenta generalizar?

`1. por x x, x es un funcion`
`2. x se le pasa un argumento, es una funcion de "un parametro" `
`3. autoAplicar :: ( ? -> ? ) -> ?`
`4. por x x, x tiene que ser capaz de recibir una funcion de tipo x por parámetro. Entonces si x :: a -> ?, a == a -> ?`
`5. entonces x :: a -> ?  ==  (a -> ?) -> ?  ==  ( (a -> ?) -> ?) -> ? == (((a -> ?) -> ?) -> ?) -> ? .....`

Sin embargo.... si pudieramos desactivar el chequeo de tipos.... esto tiene sentido?

`autoAplicar id`
`id id`
`id`

Sí!

Es más, si sólo lo desactivaramos para autoAplicar:

`autoAplicar id <- no tipa`
`id id <- tipa!!`
`id`
