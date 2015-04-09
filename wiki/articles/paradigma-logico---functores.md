Los functores son individos que nos permiten agrupar otros individuos para formar una abstracción más compleja. Tienen un nombre y una aridad determinada, si alguno de estos elementos de un functor difiere con otro pasa a ser una abstracción diferente.

Ejemplo
-------

    vende(pepe, tornillo(5,parker)).
    vende(tony, canilla(redonda,hierro,azul)).

Son valores, no predicados
--------------------------

Los functores son valores, individuos.

OK, son individuos compuestos, pero siguen siendo individuos. **No** son predicados, no tienen un valor de verdad.

Si bien existe una similitud sintáctica entre ellos, la forma de uso es distinta, es por el contexto que Prolog decide si tratarlo como definición de un hecho, como consulta de un predicado o efectivamente como un functor.

No tiene sentido consultar por functores, p.ej. hacer esta consulta

`   ?- canilla(X,hierro,Y).`

sería lo mismo que preguntar

`   ?- 1.`

Si hacemos la consulta

`   ?- canilla(X,hierro,Y).`

Y no existe un predicado canilla/3, Prolog va a lanzar un error al intentar ejecutarlo como tal y no encontrar una definición.

Si definimos lo siguiente en nuestra base de conocimientos:

`   vende(pepe, canilla(Forma,Material,Color)).`
`   canilla(triangular,hierro,azul).`
`   canilla(triangular,porcelana,blanco).`

Y luego consultamos qué cosas vende pepe, la única respuesta que puede proveernos es basura con forma de canilla:

`   ?- vende(pepe, CosaQueVende).`
`   CosaQueVende = canilla(_G9, _G10, _G11).`

En esas tres líneas de nuestra base de conocimientos tenemos predicado vende/2 que usa un functor canilla/3, y dos hechos para el predicado canilla/3. No hay ninguna relación entre ellos porque Prolog interpreta a los argumentos de los predicados como individuos. No va a intentar evaluar el functor canilla/3, lo va a tomar como un patrón para unificar en la consulta por pattern matching.

¿Cómo devolver functores?
-------------------------

Antes que nada, el uso de la palabra "devolver", marca que faltan entender cosas, porque la pregunta "cómo devuelvo" no tiene respuesta, no se puede devolver, no existe, es otro paradigma.

Si lo comparamos con Pascal, es como si uno preguntara, ¿cómo hago para que sea inversible mi procedimiento?, no tendría sentido, ¿no?.

No traten de "devolver" cosas, sino de establecer relaciones entre ellas.

Una forma de arrancar es asumir que el parámetro me viene, entonces yo trato de describir cuáles son las condiciones que se tienen que cumplir.

Una vez llegado a ese punto, si se necesita que sea inversible, analizamos cómo se van ligando las variables y si falta generar los valores posibles en algún caso.
