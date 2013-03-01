Los functores son individos que nos permiten agrupar otros individuos para formar una abstracción más compleja. Tienen un nombre y una aridad determinada, si alguno de estos elementos de un functor difiere con otro pasa a ser una abstracción diferente.

Ejemplo
-------

    vende(pepe, tornillo(5,parker)).
    vende(tony, canilla(redonda,hierro,azul)).

Son valores, no predicados
--------------------------

Los functores son valores, individuos.

OK, son individuos compuestos, pero siguen siendo individuos. **No** son predicados, no tienen un valor de verdad.

No tiene sentido consultar por functores, p.ej. hacer esta consulta

`   ?- canilla(X,hierro,Y)`

sería lo mismo que preguntar

`   ?- 1`

¿Cómo devolver functores?
-------------------------

Antes que nada, el uso de la palabra "devolver", marca que faltan entender cosas, porque la pregunta "cómo devuelvo" no tiene respuesta, no se puede devolver, no existe, es otro paradigma.

Si lo comparamos con Pascal, es como si uno preguntara, ¿cómo hago para que sea inversible mi procedimiento?, no tendría sentido, ¿no?.

No traten de "devolver" cosas, sino de establecer relaciones entre ellas.

Una forma de arrancar es asumir que el parámetro me viene, entonces yo trato de describir cuáles son las condiciones que se tienen que cumplir.

Una vez llegado a ese punto, si se necesita que sea inversible, analizamos cómo se van ligando las variables y si falta generar los valores posibles en algún caso.
