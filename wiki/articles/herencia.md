La herencia es un mecanismo que tiene por objetivo principal el compartir código similar. Esto lleva a evitar la duplicación de lógica/código.

Herencia Simple
---------------

Una clase tiene siempre una superclase pero solo una.

Lenguajes que implementan este tipo de herencia: Smalltalk, Java, C\#

Herencia Múltiple
-----------------

Una clase puede tener más de una superclase.

Lenguajes que implementan este tipo de herencia: C++, Eiffel

Generalización
--------------

Empezamos con dos clases Golondrina y Picaflor, definimos métodos para ambos

<code>

`    Golondrina >> energia`
`         ^energia`

</code>

<code>

`    Golondrina >> come: gramos`
`         "Una golondrina aumenta su energia en cuatro veces los gramos ingeridos"`
`         energia := energia + (gramos * 4)`

</code>

<code>

`    Golondrina >> vola: kms`
`         "Una golondrina disminuye su energia en 1 Joule por cada kilometro recorrido + 10 Joules que utiliza para el despegue "`
`         energia := energia - (kms + 10)`

</code>

<code>

`    Picaflor >> energia`
`         ^energia`

</code>

<code>

`    Picaflor >> come: gramos`
`         "Un picaflor aumenta su energia en cuatro veces los gramos ingeridos"`
`         energia := energia + (gramos * 4)`

</code>

<code>

`    Picaflor >> vola: kms`
`         "Un picaflor disminuye su energia en 1 Joule por cada kilometro recorrido + 20 Joules que utiliza para el despegue "`
`         energia := energia - (kms + 20)`

</code>
