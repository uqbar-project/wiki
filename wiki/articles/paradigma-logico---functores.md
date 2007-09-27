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
