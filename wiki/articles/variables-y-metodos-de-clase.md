¿Que son las variables de clase? ¿Para que sirven?
--------------------------------------------------

Las clases son objetos, y como todos los objetos pueden tener atributos y ademas les podemos mandar mensajes. Pero las clases son objetos con responsabilidades especiales, la mas grosa es la de crear nuevos objetos. Las variables de clase nos sirven cuando queremos que nuestros objetos tengan alguna referencia a algun valor, que sea el mismo para todas las instancias de esa clase, y que ademas pueda cambiar (por eso usamos una variable y no una constante). Si usaramos una constante, si yo cambio ese valor despues de haber creado un objeto, el objeto no se entera que yo lo cambie, porque “recuerda” el estado que tenia cuando se creo; entonces si yo quiero que se entere de lo que yo cambie tengo que volver a crearlo... Y si tengo muchos objetos eso no esta bueno :O. En cambio si tengo una variable de clase y cambio el valor al que referencia, automaticamente todas las instancias que yo cree a partir de esa clase en la vida van a conocer ese nuevo valor, porque lo conoce su clase y ellas conocen a su clase :)

Diferencia con las variables de instancia
-----------------------------------------

Las variables de instancia son propias de cada objeto, si cambio una referencia en un objeto la referencia de otro de la misma clase no cambia (aunque las variables se llamen igual). En cambio, si cambio la referencia de una variable de clase, cambia para todas las instancias.

¿Como las usamos?
-----------------

Las variables de clase empiezan con mayúscula. Suponiendo que el dinero de todos los saqueos es el mismo, podemos ponerlo en una variable de clase:

Por ejemplo:

` #Saqueo`
`   >>esUtil: unPirata`
`     ^(unPirata dineroDisponible < `**`Dinero`**`) and: [ unPirata teAnimasASaquear: objetivo].`

Pero, como el Dinero es algo que en realidad "está guardado" en la clase, (y por lo tanto es responsabilidad de la clase conocerlo), una versión más correcta (y que también funciona) es:

` #Saqueo`
`   >>esUtil: unPirata`
`     ^(unPirata dineroDisponible < `**`Saqueo`**` dinero) and: [ unPirata teAnimasASaquear: objetivo].`
` `**`(MC)>>` `dinero`**
`      `**`^Dinero`**

Por último, una forma más correcta sería poniendo **`self` `class`**, de esta manera:

` #Saqueo`
`   >>esUtil: unPirata`
`     ^(unPirata dineroDisponible < `**`self` `class`**` dinero) and: [ unPirata teAnimasASaquear: objetivo].`
` (MC)>> dinero`
`      ^Dinero`

La razón por la que es más correcta, tiene que ver con la existencia de [Herencia](herencia.html), y otros mecanismos de compartición de código.

Mensajes y Métodos de clase
---------------------------

Ente otras cosas, las clases crean objetos nuevos: cada vez que a le mando el mensaje **new** a una clase, tengo una nueva instancia de esa clase. Podemos crear nuevas instancias en cualquier momento de nuestro programa (en el workspace, dentro de un método, etc).

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
