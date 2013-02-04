Son funciones anónimas. Por ej., una función anónima que toma solo un número como argumento y produce el resultado x + x se puede definir como:

`   λx → x + x`

En Haskell:

λ se escribe \\ → se escribe -&gt;

Esa definición no es más que una función doble, por eso podemos escribir

doble = λx → x + x

A pesar de no tener nombre, pueden usarse como cualquier otra función:

&gt;(λx → x + x) 2 4

Una expresión lambda puede recibir más de 1 parámetro separándolos por espacio dentro de la expresión

--cuentaLoca es una función que recibe 3 parámetros cuentaLoca = (\\x y z -&gt; x \* 2 - y + 10 \* z)
