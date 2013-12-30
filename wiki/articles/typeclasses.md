Problema
--------

Si tenemos la multiplicacion (\*) definida para tanto Int como Float, luego

square x = x\*x

debería transformarse en tiempo de compilación en dos funciones distintas

square (para ints) square (para floats)

Lo cual crece exponencialmente, por ejemplo con funciones como

squares (x,y,z) = (x\*x, y\*y, z\*z)

que sería traducida en 8 funciones distintas.

Typeclasses al rescate
----------------------

Las typeclasses son un contrato o tipo de datos abstracto que agrupa las funciones sobrecargadas: es decir, definen una lista de funciones que un tipo deber implementar para considerarse de esa clase. Ejemplo: la typeclass Num dice que todos aquellos tipos que sean Num (Int o Float por ejemplo) van a definir las funciones (+) y (\*) y de que tipos son.

Las typeclasses tienen como primer consecuencia que funciones como squares, que tenía 8 tipos posibles, tenga uno solo:

squares :: Num a, Num b, Num c =&gt; (a,b,c) -&gt; (a,b,c)

Donde Num es un typeclass.

Finalmente comenta como las typeclasses se pueden traducir/reescribir a un lenguaje sin typeclasses. Y por lo tanto, como pueden reutilizarse los mecanismos de inferencia de tipos de uno para el otro.

Y para esta traducción, usan la metáfora de un method dictionary. Cada tipo va a tener un diccionario con sus funciones sobrecargadas. Luego, cada función con \*polimorfismo parametrico\*, recibe como parámetro el method dictionary y usa la función adecuada:

Ejemplo:

square x = x \* x

se transforma en algo como

--ambos diccionarios son del mismo tipo (que es el tipo definido en la typeclass ;) ) dictInt = ... dictFloat = ....

multi dict = dict !! 1 -- suponiendo que la función multiplicación esta en el diccionario en la posición 1 square dict x = multi dict x x

Y cuando ejecutamos

square 3

se traduce, como 3 es un Int, en algo como

square dictInt 3

En resumen: chequeo estático, dispatch dinámico :P (o algo dinámico)
