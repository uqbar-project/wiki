Cuando hablamos de currificación nos referimos a que todas las funciones reciben un único parámetro como máximo. El hecho de que sea posible definir funciones de más de un parámetro se debe a que son funciones currificadas. Cuando evaluamos por ejemplo, max 4 5, lo que sucede es que se le aplica el número 5 a la función resultante de aplicarle el 4 a max, o sea que se transforma en (max 4) 5

Por eso, cuando escribimos el tipo de una función no hay una distinción entre lo que son los parámetros y el valor de retorno, se desdibuja un poco la diferencia. Por ejemplo, el tipo de max puede escribirse de dos formas:

max :: (Ord a) =&gt; a -&gt; a -&gt; a &lt;--- Forma tradicional

max :: (Ord a) =&gt; a -&gt; (a -&gt; a) &lt;--- Forma currificada

La segunda denota que max es una función de 1 parámetro que retorna una función que espera y retorna algo del mismo tipo que lo que ella espera.

Esto es lo que permite tener [Aplicación Parcial](aplicacion-parcial.html)
