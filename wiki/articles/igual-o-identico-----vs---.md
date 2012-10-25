Definiciones
------------

**Identidad:** decimos que dos objetos son idénticos si son el mismo objeto. Dentro del ambiente podemos tener dos referencias diferentes al mismo objeto. **Igualdad:** *por defecto dos* objetos son iguales si son el mismo objeto (en Object se define el mensaje = como self == anObject). La igualdad puede ser redefinida (Ver [Redefinición](redefinicion.html)) si el problema lo amerita.

Ejemplos
--------

Algunos objetos literales como los números y los símbolos (son como los strings pero arrancan con \#) son únicos en el sistema, por ese motivo evaluar \#hola == \#hola da true.

Conviene hacer las comparaciones con `=` y no con `==`.

La razón contada muy rápido: imaginate que por alguna razón en la imagen de Smalltalk (o de cualquier lenguaje que trabaja con objetos) en un momento hay dos String "hola", que están apuntados por las variables str1 y str2. La comparación

`   str1 = str2`

da true, porque representan al mismo ente. Pero la comparación

`   str1 == str2`

da false, porque no son exactamente el mismo objeto, la imagen los reconoce como distintos.

Dando un paso más, en qué casos podría ser útil redefinir el =? Hay algunos objetos que tienen como principal objetivo representar datos más allá de los literales que ya conocemos, como ser las fechas. Si yo evalúo esto en un workspace:

d1 := Date newDay: 22 month: 10 year: 1987. d2 := Date newDay: 22 month: 10 year: 1987.

d1 = d2. d1 == d2.

Esperaría que la consulta de igualdad sea cierta pero no la de identidad. Si vemos la implementación de Pharo que se encuentra en Timespan (superclase de Date) encontramos esto:

= comparand

` ^ self class = comparand class `
`      and: [ self start = comparand start `
`          and: [ self duration = comparand duration ]]`

De la misma forma podríamos redefinir la igualdad en alguna clase propia, como ser Direccion que tiene una calle y una numeración.

= otraDireccion

` ^ self class = otraDireccion class `
`      and: [ self calle = otraDireccion calle`
`          and: [ self numeracion = otraDireccion numeracion]]`

**Importante** Cuando redefinimos el = hay que redefinir también el método hash que es usado por ejemplo por las colecciones. De esa forma si queremos tener un Set de direcciones funcione como esperamos de modo que no hayan dos direcciones iguales. El hash (no sólo en objetos) es una función que retorna un número para un determinado elemento. En nuestro caso va a ser un mensaje unario que recibe el objeto al cual le redefinimos la igualdad, por ejemplo para las direcciones:

hash

`^ self calle hash + self numeracion hash`
