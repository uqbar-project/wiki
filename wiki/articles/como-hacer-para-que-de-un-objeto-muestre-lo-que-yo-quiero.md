antes que nada, otra forma de saber si lo que me devuelve es lo que quiero
--------------------------------------------------------------------------

Supongamos que tengo las clases Golondrina y Lugar, que entienden estos mensajes

-   Golondrina: energia (un número), dondeEsta (un lugar), jefaDeBandada (otra golondrina)
-   Lugar: kmEnRuta (un número)

En un workspace tengo esto

`  pepita := Golondrina new.`
`  luciana := Golondrina new.`
`  buenosAires := Lugar new.`
`  pepita initialize.`
`  luciana initialize.`
`  buenosAires initialize.`
`  buenosAires kmEnRuta: 10.`
`  pepita jefaDeBandada: luciana. "le digo a pepita que la jefa de su bandada es luciana"`
`  pepita ubicacionInicial: buenosAires.   "después de esto, pepita está en buenos aires"`

Si después de todo esto en el workspace pongo

`   pepita energia`

y le doy display it, me va a mostrar un número, p.ej. 0. Ahora, si pongo

`   pepita jefaDeBandada`

y le doy display it, no me va a mostrar "luciana" que es tal vez lo que esperaba, sino "a Golondrina". Parecido si pruebo con

`   pepita dondeEsta`

me va a decir "a Lugar".

Si tengo 5 golondrinas en mi workspace, ¿cómo sé que la que me devuelve cuando le pido la jefa de bandada a pepita es luciana y no otra? "luciana" es el nombre de la variable que apunta a luciana dentro del workspace, entonces dentro del workspace puedo usar ese nombre. Sabiendo esto, una fácil es preguntar si lo que me devuelve `pepita` `jefaDeBandada` es luciana, o sea

`   pepita jefaDeBandada = luciana`

ahora las respuestas van a ser true o false.

ahora sí, el misterio de "a Golondrina"
---------------------------------------

Ponele que estás pensando "todo bien, pero lo que quiero es que no me ponga 'a Golondrina'". Adelante.

Empecemos por entender por qué pone "a Golondrina".

Ya vimos que los objetos no tienen "nombre propio", los que tienen nombre son las variables que hacen referencia al objeto. luciana es la variable del workspace que apunta a una golondrina, la golondrina no tiene nombre propio.

Entonces, cuando pinto una expresión y le doy "display it", ¿qué me muestra? Me muestra un String que representa al objeto resultado de la expresión. Para los objetos básicos (números, String, booleanos), ese String es lo que uno espera. Para las instancias de las clases que creamos nosotros, es en principio el nombre de la clase antecedido de "a " (o "an " si el nombre de la clase empieza en vocal). Por eso "a Golondrina".

Si entendimos esto, la pregunta que sigue es ¿puedo hacer que el String que representa a (p.ej.) las golondrinas no sea "a Golondrina" sino otra cosa que yo quiera.

Sí, y no es muy difícil.

printOn:
--------

Lo que hay que hacer es definir el método

`   printOn:`

en la clase cuyas instancias queremos que se muestren distinto (en el ejemplo, Golondrina y Lugar).

Lo que viene como parámetro es un Stream, que es una tira de caracteres. En el método tenés que agregar el String que vos quieras a la tira. Todos los objetos son polimórficos respecto del printOn:, para las clases que no tienen una definición explícita, Smalltalk provee la que muestra el nombre de la clase.

El "display it" lo que hace es: crear un Stream, pedirle al resultado de la expresión printOn: sobre ese Stream, tomar el String generado, mostrar eso.

Los Stream entienden estos mensajes:

-   nextPutAll: unString , agrega unString a la tira
-   cr , agrega un salto de línea a la tira

Tengamos también en cuenta que los String entienden el mensaje coma (,) que concatena, probar p.ej.

`  'hola ', 'mundo'`

Más sobre Strings en el apunte sobre objetos básicos en www.pdep.com.ar .

Ya vimos que los objetos básicos se muestran bien, y ahora sabemos que eso es porque al decirle printOn: devuelven un String feliz. P.ej. el 42 agrega el String "42", por eso es que cuando el resultado de una operación es 42 y le pido "display it" de esa operación, me muestra "42" y no "a Number". Esto lo podemos usar, si dentro de lo que quiero mostrar hay p.ej. un número, entonces al número le puedo decir printOn: sobre el mismo Stream que me pasaron a mí.

Hagamos que los lugares y golondrinas se muestren bien

`   #Lugar`
`   printOn: unStream`
`       unStream nextPutAll: 'lugar en km '.`
`       self kmEnRuta printOn: unStream             `

la segunda línea de código agrega el String correspondiente al número en el mismo Stream que le llegó al lugar, entonces el resultado va a ser p.ej. "lugar en km 10".

`   #Golondrina`
`   printOn: unStream`
`       unStream nextPutAll: 'golondrina con energía '.`
`       self energia printOn: unStream.`
`       unStream nextPutAll: ' y que está en ('.`
`       self dondeEsta printOn: unStream.`
`       unStream nextPutAll: ')'.`

acá usé el mismo truco para la energía (un número) y para el lugar, para el que va a usar el printOn: de lugar que definimos recién. Entonces el resultado va a ser "golondrina con energía 0 y que está en (lugar en km 10)".

ok, pero quiero que me diga "pepita"
------------------------------------

Bueno, para eso el objeto tiene que conocer al String 'pepita'. P.ej. que las golondrinas tengan nombre. ¿Cómo? Bueno, eso ya lo deberían poder hacer ustedes ;-).
