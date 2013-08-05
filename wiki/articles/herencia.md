La herencia es un mecanismo que tiene por objetivo principal el compartir lógica/código similar. Esto lleva a evitar la duplicación de lógica/código. Cuando un objeto recibe un mensaje, mediante [Method lookup](method-lookup.html) buscará el comportamiento requerido en la clase de la cual es instancia y, en caso de no tener un método para el mismo, en sus superclases.

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

Empezamos con dos clases, Golondrina (con una variable de instancia energia) y Picaflor (con una variable de instancia energia) , definimos métodos para ambos

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

En Smalltalk la forma de crear una clase es enviándole el mensaje \#subclass:instanceVariableNames:classVariableNames: (o uno similar, depende del dialecto de Smalltalk utilizado) a la superclase de la clase que queremos crear.

<code>

`    Superclass`
`         subclass: #NameOfSubclass`
`         instanceVariableNames: 'variableInstancia1 variableInstancia2 variableInstanciaN'`
`         classVariableNames: 'variableClase1 variableClase2 variableClaseN'`

<code>

En nuestro ejemplo

<code>

`    Object`
`         subclass: #Ave`
`         instanceVariableNames: 'energia'`
`         classVariableNames: ''.`

`    Ave `
`         subclass: #Golondrina`
`         instanceVariableNames: ''`
`         classVariableNames: ''.`

`    Ave `
`         subclass: #Picaflor`
`         instanceVariableNames: ''`
`         classVariableNames: ''.`

</code>

Clase abtracta
--------------

En el ejemplo anterior la clase \#Ave se está usando como superclase de \#Golondrina y \#Picaflor. Si nosotros queremos instanciar un ave deberíamos elegir si será una golondrina o un picaflor para mandarle el mensaje new a alguna de esas clases. Lo que no sería correcto es:

`unAve := Ave new.`

Este código va a funcionar en principio, pero si yo considero que un ave tiene que poder volar y no hay una implementación en \#Ave para esta operación ya que está definido de formas diferentes en sus subclases, unAve no va a entender el mensaje.

Entonces, una clase abstracta es aquella que no tiene sentido instanciar porque es demasiado genérica y no tiene una implementación concreta para algunos mensajes que debería entender porque está definido en sus subclases.
