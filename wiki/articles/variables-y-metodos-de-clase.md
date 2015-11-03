Mensajes y Métodos de clase
---------------------------

Las clases son objetos, y como todos los objetos pueden tener atributos y ademas les podemos mandar mensajes. Pero las clases son objetos con responsabilidades especiales, la más común es la de crear nuevos objetos. Cada vez que a le mando el mensaje **new** a una clase obtengo una nueva instancia de esa clase. Podemos crear nuevas instancias en cualquier momento de nuestro programa (en el workspace, dentro de un método, etc).

Entonces, los mensajes de clase son mensajes que entienden las clases, no las instancias de las mismas. El nombre (selector) de un mensaje de clase se escribe igual que siempre (empieza con minúscula).

Algunos ejemplos:

Workspace:

`  pepita:= Golondrina new.`

`new` es uno de los primeros mensajes de clase que aprendemos: nos devuelve una nueva instancia de la clase que recibe el mensaje. En este caso, devuelve una golondrina.

Workspace:

`  hoy := Date today.`

`today` es un mensaje de clase que le mando a la clase Date y me devuelve un objeto que representa la fecha de hoy.

Workspace:

`  items:= Bag with: 'brujula' with: 'botellaDeGrogXD' with: 'catalejo'.`

`with:with:with:` es un mensaje que entienden las clases de colecciones (en este caso Bag es la clase que recibe el mensaje). `with:with:with:` devuelve una nueva colección, que tiene los tres elementos adentro. En éste caso, devuelve un bag con la brújula, la botella y el catalejo.

En general es deseable tener una forma de crear objetos que ya estén inicializados adecuadamente para poder usarlos inmediatamente, y no que arranquen en un estado inválido y requieran ser configurados a posteriori.

### ¿Cómo escribo métodos de clase?

Cada mensaje de clase debe tener asociado un método de clase (su codificación).

-   En Pharo: En el segundo panel del System Browser, apretando el boton **class**, escriben el método como hasta ahora. (No se olviden de volver a apretar **instance** para escribir metodos de instancia)
-   En el parcial: simplemente pongan **(MC)** al lado del método, para diferenciarlo de los métodos de instancia.

Workspace:

`  barbanegra := Pirata nuevoConItems: items ebriedad: 100 monedas: 1500.`

El mensaje `nuevoConItems:ebriedad:monedas:` se lo mando a la clase Pirata y me devuelve un nuevo pirata ya inicializado con los items, la ebriedad y las monedas que le indiquemos.

Codificación:

` #Pirata`
` (MC)  >> nuevoConItems: losItems ebriedad: nivelEbriedad monedas: unasMonedas`
`    |unPirata|`
`  unPirata:= self new.`
`  unPirata items: losItems.`
`  unPirata ebriedad: nivelEbriedad.`
`  unPirata monedas: unasMonedas.`
`  ^unPirata.`

Recordemos que, como el que recibe este mensaje es una clase, acá **self es la clase**, no una instancia. El motivo por el cual es importante usar self en vez de escribir el nombre de la clase (en este caso Pirata) es porque estos métodos también se heredan, y si subclaseamos Pirata no se va a comportar como queremos cuando le mandemos `nuevoConItems:ebriedad:monedas:` a la subclase. Al usar self indicamos que el mensaje debe enviarse al objeto que recibió `nuevoConItems:ebriedad:monedas:`, o sea que si se lo mandamos a una subclase de Pirata, la instancia creada será de dicha clase.

¿Que son las variables de clase? ¿Para que sirven?
--------------------------------------------------

Las variables de clase nos sirven cuando queremos que nuestros objetos tengan alguna referencia a algún valor, que sea el mismo para todas las instancias de esa clase, y que ademas pueda cambiar (por eso usamos una variable y no hardcodeamos ese valor en el código del programa, asumiendo que es posible).

Si usáramos una variable de instancia con la intención de settear el mismo valor a todas las instancias de esa clase, y luego queremos cambiar el valor de modo de afectar a todas las instancias, tengo que poder encontrar todas las instancias ya existentes para poder mandarles el mensaje para que actualicen su referencia... Y si tengo muchos muchos objetos eso no esta bueno, no sólo por la complejidad innecesaria del problema, sino también porque la performance puede verse afectada. Este problema desaparece si tengo un único objeto que conozca este valor que quiero que varios objetos conozcan, y como todo objeto conoce la clase a la que pertenece, podemos dejar que sea responsabilidad de la clase recordar y manipular este estado, y que sus instancias usen ese valor cuando lo necesiten.

Si tengo una variable de clase y cambio el valor al que referencia, automáticamente todas las instancias que yo cree a partir de esa clase en la vida van a conocer ese nuevo valor, porque lo conoce su clase y ellas conocen a su clase :)

Diferencia con las variables de instancia
-----------------------------------------

Las variables de instancia son propias de cada objeto, si cambio una referencia en un objeto la referencia de otro de la misma clase no cambia (aunque las variables se llamen igual). Si cambio la referencia de una variable de clase, todas las instancias van a verse afectadas, ya que no es una referencia propia.

En Smalltalk, para declarar una variable de instancia (por ejemplo, dada una clase Pirata, para decir que cada pirata conoce cuántas monedas tiene) debíamos indicarlo de esta forma:

`Object subclass: #Pirata`
`   instanceVariableNames: 'monedas'`
`   classVariableNames: ''`
`   category: 'Yaaaar'`

Para declarar una variable de clase en cambio, el nombre de la variable debe ir entre las comillas que siguen a classVariableNames:, por convención inician con mayúscula. Suponiendo que el dinero máximo que tiene que tener un pirata para poder saquear un objetivo es igual para todas las misiones de saqueo es el mismo, podemos ponerlo en una variable de clase

`Object subclass: #Saqueo`
`   instanceVariableNames: ''`
`   classVariableNames: 'DineroMaximo'`
`   category: 'Yaaaar'`

¿Como las usamos?
-----------------

Dentro de un método de instancia de Saqueo podemos directamente usar una variable de clase definida para la clase Saqueo, por ejemplo:

` #Saqueo`
`   >>esUtil: unPirata`
`     ^(unPirata dineroDisponible < `**`DineroMaximo`**`) and: [ unPirata teAnimasASaquear: objetivo].`

Pero, como el DineroMaximo es algo que en realidad "está guardado" en la clase, (y por lo tanto es responsabilidad de la clase conocerlo), otra alternativa es mandarle un mensaje a la clase:

` #Saqueo`
`   >>esUtil: unPirata`
`     ^(unPirata dineroDisponible < `**`Saqueo`**` dineroMaximo) and: [ unPirata teAnimasASaquear: objetivo].`
` `**`(MC)>>` `dineroMaximo`**
`      `**`^DineroMaximo`**

Por último, una forma más correcta sería poniendo **`self` `class`**, de esta manera:

` #Saqueo`
`   >>esUtil: unPirata`
`     ^(unPirata dineroDisponible < `**`self` `class`**` dineroMaximo) and: [ unPirata teAnimasASaquear: objetivo].`
` (MC)>> dineroMaximo`
`      ^DineroMaximo`

La razón por la que es más correcta, tiene que ver con la existencia de [Herencia](herencia.html) como se mencionó anteriormente. Así damos la posibilidad a una subclase a redefinir dineroMaximo.
