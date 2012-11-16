Que hace la siguiente función?

autoAplicar funcion = funcion funcion

OK, y que tipo tiene?

autoAplicar :: (a-&gt;a) -&gt; (a-&gt;a) ??

f :: (a-&gt;a) -&gt; ( a -&gt; a) f x = x x

`   Occurs check: cannot construct the infinite type: a = a -> a`
``    When generalising the type(s) for `f' ``

A que intenta generalizar?

1. por x x, x es un funcion 2. x se le pasa un argumento, es una funcion de "un parametro" 3. autoAplicar :: ( ? -&gt; ? ) -&gt; ? 4. por x x, x tiene que ser capaz de recibir una funcion de tipo x por parámetro. Entonces si x :: a -&gt; ?, a == a -&gt; ? 5. entonces x :: a -&gt; ? == (a -&gt; ?) -&gt; ? == ( (a -&gt; ?) -&gt; ?) -&gt; ? == (((a -&gt; ?) -&gt; ?) -&gt; ?) -&gt; ? .....

Sin embargo.... si pudieramos desactivar el chequeo de tipos.... esto tiene sentido?

autoAplicar id id id id

Sí!

Es más, si sólo lo desactivaramos para autoAplicar:

autoAplicar id &lt;- no tipa id id &lt;- tipa!! id
