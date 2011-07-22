¿Que son las variables de clase? ¿Para que sirven?
--------------------------------------------------

Las clases son objetos, y como todos los objetos pueden tener atributos y ademas les podemos mandar mensajes. Pero las clases son objetos con responsabilidades especiales, la mas grosa es la de crear nuevos objetos. Las variables de clase nos sirven cuando queremos que nuestros objetos tengan alguna referencia a algun valor, que sea el mismo para todas las instancias de esa clase, y que ademas pueda cambiar (por eso usamos una variable y no una constante). Si usaramos una constante, si yo cambio ese valor despues de haber creado un objeto, el objeto no se entera que yo lo cambie, porque “recuerda” el estado que tenia cuando se creo; entonces si yo quiero que se entere de lo que yo cambie tengo que volver a crearlo... Y si tengo muchos objetos eso no esta bueno :O. En cambio si tengo una variable de clase y cambio el valor al que referencia, automaticamente todas las instancias que yo cree a partir de esa clase en la vida van a conocer ese nuevo valor, porque lo conoce su clase y ellas conocen a su clase :)

Diferencia con las variables de instancia
-----------------------------------------

Las variables de instancia son propias de cada objeto, si cambio una referencia en un objeto la referencia de otro de la misma clase no cambia (aunque las variables se llamen igual). En cambio, si cambio la referencia de una variable de clase, cambia para todas las instancias.

¿Como las usamos?
-----------------

Las variables de clase empiezan con mayuscula, y si queremos jugar con los objetos que referencian tenemos que usar accesors (no esta bueno usar directamente la variable desde la instancia porque no es algo que conoce la instancia, sino que la conoce la clase).

Por ejemplo:

` #Saqueo`
`   >>esUtil: unPirata`
`     ^(unPirata dineroDisponible < Saqueo dinero) and: [ unPirata teAnimasASaquear: objetivo].`
` (MC)>> dinero`
`      ^Dinero`
` (MC)>> dinero: unaCantidad`
`      Dinero:= unaCantidad`

Métodos de clase
----------------

Ente otras cosas, las clases crean objetos nuevos: cada vez que a le mando el mensaje **new** a una clase, tengo una nueva instancia de esa clase. Podemos crear nuevas instancias en cualquier momento de nuestro programa (en el workspace, dentro de un método, etc).

Entonces, los métodos de clase son mensajes que entienden las clases. Los selectores se escriben igual que siempre (empiezan con minúscula).

¿Cómo escribo métodos de clase?

-   En Pharo: En el segundo panel del System Browser, apretando el boton **class**, escriben el metodo como hasta ahora. (No se olviden de volver a apretar **instance** para escribir metodos de instancia)
-   En el parcial: simplemente pongan **(MC)** al lado del metodo, para diferenciarlo de los métodos de instancia.

Algunos ejemplos:

` #Pirata`
` (MC)`
` >> nuevoConItems: losItems ebriedad: nivelEbriedad monedas: unasMonedas`
`    |unPirata|`
`  unPirata:= self new.`
`  unPirata items: losItems.`
`  unPirata ebriedad: nivelEbriedad.`
`  unPirata monedas: unasMonedas.`
`  ^unPirata.`

`  items:= Bag with: 'brujula' with: 'botellaDeGrogXD' with: 'catalejo'.`

`  Date today.`

`  pepita:= Golondrina new.`
