Por si les sirve les comento un toque como funciona el tema de los arrays en smalltalk.

Acuérdense que en Smalltalk vale usar arrays literales.

  
Array: colección de tamaño **fijo**.

Literal: objetos especiales como números, caracteres, strings, nil, booleanos, arrays literales (cuac), etc.

Para usar un Array literal los elementos del array deben ser literales, ejemplos:

`  #('brujula' 'mapa' 'botellaDeGrogXD')`

Escribir eso, genera un array que tiene 3 strings, si necesitan un Bag pueden escribir

`  #('brujula' 'mapa' 'botellaDeGrogXD') asBag`

El tema está cuando queremos un array que tiene objetos que deben obtenerse a través de mensajes, ejemplo:

`  #(4+3)`

Uno ingenuamente se pensaría que eso da un array de un elemento (el objeto 7), pero Smalltalk piensa que quisimos hacer un array que tiene 3 elementos (el 4, el símbolo +, y el 3)

Para crear arrays con objetos que no son literales, se deben utilizar arrays dinámicos (en vez de escribir \#() se escribe { }, en vez de separarse los elementos con espacio se separan con punto)

`  {4 + 3} "Esto es un array con un solo elemento, el número 7"`

`  {4 + 3 . 'hola' size . Date today year} "Esto es un array con tres elementos: 7, 4 y 2011"`

De todas formas, en los parciales no suele ser habitual el uso de Arrays (ya que queremos colecciones con tamaño variable y los elementos que componen dicha colección son desconocidos al escribir el código).
