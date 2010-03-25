La herencia es un mecanismo que tiene por objetivo principal el compartir lógica/código similar. Esto lleva a evitar la duplicación de lógica/código.

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

Empezamos con dos clases Golondrina (con una variable de instancia energia) y Picaflor (con una variable de instancia energia) , definimos métodos para ambos

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

Lo que nos permite la idea de generalización utilizando herencia es crear nuevas abtracciones.

En el código de arriba nos podemos dar cuenta que tanto las golondrinas como los picaflores saben decirnos su energia, comer y volar.

Ahora bien, las golondrinas y los picaflores (por ejemplo) saben comer pero además comen de la misma forma. Estaría bueno poder generalizar eso, si las únicos pajaritos con los que estoy trabajando son golondrinas y picaflores puedo decir que todas las aves comen de la misma forma. Entonces generalizo el concepto de Golondrina y Picaflor utilizando una nueva abstracción, como necesito poner en esa abstracción métodos y definir atributos nada mejor que esa nueva abstracción sea una nueva clase

<code>

`    Ave >> come: gramos`
`         "Un ave aumenta su energia en cuatro veces los gramos ingeridos"`
`         energia := energia + (gramos * 4)`

</code>

Pero no puedo poner ese código en la clase Ave porque esa clase no tiene una variable de instancia energia.

Si todas las aves tienen que tener una variable de instancia es algo que me gustaría dejar escrito solo en Ave.

`Ave` `tiene` `definida` `una` `variable` `de` `instancia` `energia`

¿Cómo sigue esto?

Tengo que explicitar que las golondrinas tienen todo el comportamiento que esta en la clase Golondrina y también tienen el comportamiento que está en la clase Ave. Además tengo que explicitar que los picaflores tienen todo el comportamiento que esta en la clase Picaflor y también tienen el comportamiento que está en la clase Ave.

Esto se hace diciendo que Ave es superclase de Golondrina y Ave es superclase de Picaflor; además tenemos que eliminar el código repetido de las clases Golondrina y Picaflor.
