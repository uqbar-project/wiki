Supongamos que queremos extender el viejo y querido ejemplo de Pepita la golondrina para que vuele o coma en base a su salud. Cuando está cansada no puede volar, cuando está hambrienta y se le da de comer aumenta el doble de energía que cuando está normal y antes de volar come un poquito también, cuando está enérgica gasta más energía al volar que lo normal y vuela un poquito antes de comer...

Sería posible resolver toda esta lógica (y la que esté por venir más adelante) con muchos ifs en pepita, pero es posible modelarlo de otra forma: los diferentes estados de salud de pepita podrían ser otros objetos separados que le ayuden a saber cómo debería volar y comer, y por su puesto ser polimórficos para que pepita pueda delegar en ellos esta funcionalidad sin importar cuál sea su estado actual (objeto al cual referencia con algún atributo propio).

Pasamos de tener *un objeto que resuelve todo el problema* a *un objeto que conoce a otros objetos polimórficos para resolver el problema mediante la colaboración*. Con esta solución, el flujo del programa ya no se encuentra definido por los ifs y objetos básicos sino por la configuración de pepita y el uso de [polimorfismo](polimorfismo.html).

Entonces, la composición en objetos es simplemente una relación de conocimiento entre dos objetos (por ejemplo, pepita conoce a su estado de salud) donde el objeto conocido puede cambiarse por otro que sea polimórfico para el que los conoce.

Otro ejemplo podría ser el de las colecciones con un algoritmo de ordenamiento elegido por el usuario ([SortedCollection](sabores-de-colecciones.html) en Smalltalk), donde la colección delega en otro objeto que modela el algoritmo de ordenamiento a usar sobre sus elementos.

El uso de composición en ocasiones es una solución muy elegante para problemas aparejados por el concepto de [Herencia](herencia.html), que pueden verse en el siguiente ejemplo tomado de un final de Paradigmas de Programación:

Cambiando herencia por composición
----------------------------------

El siguiente texto representa parte del relevamiento realizado en una cadena de venta de electrodomésticos: “Los vendedores pueden ser especialistas o de salón. Los especialistas atienden detrás de mostrador y cobran un premio (todos los especialistas cobran el mismo monto) por cada venta mayor a 500 pesos. Los vendedores de salón cobran un premio (diferente para cada vendedor) si hacen más de 50 ventas "

Avanzando en el relevamiento, nos dicen lo siguiente:

"Para motivar las ventas en el equipo, decidimos incorporar un cambio: categorías senior y junior. Un vendedor senior tendrá a cargo a un junior. Un vendedor senior recibe como parte del premio un adicional correspondiente al 3% de la las ventas realizadas por la persona que tiene a cargo. Un Junior tiene un porcentaje de descuento en su premio, diferente para cada uno. Por otra parte, si un vendedor junior hace bien las cosas, con el tiempo puede pasar a ser senior" "

La codificación propuesta en el enunciado es:

[Diagrama de clases propuesto](http://yuml.me/0dd23abc)

`#VendedorEspecialista`
` >>premio`
`   ^ self class premio`

`#VendedorSalon`
` >>premio`
`   ^ premio`

`#VendedorSalonSenior`
` >>premio`
`   ^ super premio + self adicionalJunior`
` >>adicionalJunior`
`   ^ junior totalVentas * 0.03.`

`#VendedorSalonJunior`
` >>totalVentas`
`   ^ ventas inject: 0 into: [ :total :venta | total + venta monto ].`
` >>premio`
`   ^ super premio * (1- self descuento)`
`#VendedorEspecialistaSenior`
` >>premio`
`   ^ super premio + self adicionalJunior.`
` >>adicionalJunior`
`   ^ junior totalVentas * 0.03.`

`#VendedorEspecialistaJunior`
` >>totalVentas`
`   ^ ventas inject: 0 into: [ :total :venta | total + venta monto ].`
` >>premio`
`   ^ super premio * (1- self descuento)`

La solución propuesta tiene problemas que surgen por el mal uso de herencia. Los que podemos destacar son:

-   **Repetición de código:** La forma de subclasificar a los vendedores tanto por tipo de vendedor (Salón o Especialista) como por categoría (Senior o Junior) hace que tengamos código repetido entre las hojas del árbol de herencia. Esto tiene problemas, sobre todo si el sistema sigue creciendo de esta forma, ya que la repetición de código es exponencial y realizar un cambio en la lógica del premio de los juniors por ejemplo se propagaría para todos los tipos de vendedores habidos y por haber (tiene problemas de **[extensibilidad](atributos-de-calidad.html)**).
-   **Problemas con la [identidad](igual-o-identico-----vs---.html) de los objetos:** El enunciado indica que un junior puede volverse senior con el tiempo, pero el modelo que tenemos no soporta este tipo de cambio en tiempo de ejecución. Un objeto de la clase VendedorSalonJunior no puede cambiar de clase a VendedorSalonSenior, su clase es algo que se mantiene durante toda la vida del objeto. Si tratamos de emular el cambio de clase creando un nuevo objeto y copiando los valores de sus atributos según corresponda lograremos tener el comportamiento de senior pero ya no será el mismo objeto para el sistema. En OOP una de las características de un objeto es su identidad, la cual estaríamos perdiendo si tomamos esa decisión y el problema asociado a este cambio es que todos los objetos que tengan una referencia a nuestro vendedor promovido deberán enterarse de este cambio (y seguramente no lo hagan) para referenciar al nuevo objeto que lo reemplaza. La consecuencia de esto es o bien una complejidad espantosa para mantener las referencias o un sistema **[inconsistente](atributos-de-calidad.html)**.

**¿Cómo se soluciona este problema?** Si cambiamos el modelo para que la categoría (Junior o Senior) sea un objeto aparte que el vendedor conozca y delegamos en este objeto todo aquello que corresponda a ser senior o junior solucionamos ambos problemas a la vez, ya que el valor de las referencias sí puede ser cambiado en tiempo de ejecución, es sólo settear un atributo. Veamos cómo queda la nueva solución:

[Diagrama de la nueva solución](http://yuml.me/b266b1e2)

`#Vendedor`
` >>premio`
`   ^ self categoria premioPara: self`
` >>totalVentas`
`   ^ ventas inject: 0 into: [ :total :venta | total + venta monto ].`

`#Senior`
` >>premioPara: unVendedor`
`   ^ unVendedor premioBase + self adicionalJuniorPara: unVendedor`
` >>adicionalJuniorPara: unVendedor`
`   ^ junior totalVentas * 0.03.`

`#Junior`
` >>premioPara: unVendedor`
`   ^ unVendedor premioBase * (1- self descuento)`

`#VendedorEspecialista`
` >>premioBase`
`   ^ self class premioBase`

`#VendedorSalon`
` >>premioBase`
`   ^ premioBase`

**Disclaimer:** el mensaje \#totalVentas fue a parar al vendedor porque tenía sentido para todos, no sólo para los juniors, y era más simple pero para que fuera totalmente análoga podríamos tenerlo definido en \#Junior y delegar en la categoría. En caso de dudas siempre vale preguntar.

Como se puede ver en el diagrama de clases de la solución con composición, para crear un vendedor ya no alcanza sólo con elegir la clase del tipo de vendedor que queremos e instanciarla, sino que tenemos que instanciar dos objetos (al vendedor que queramos y su categoría) y hacer que el vendedor conozca a su categoría, lo cual agrega una **[complejidad](atributos-de-calidad.html)** extra para la creación de nuestros objetos. Si más adelante quisiéramos que un vendedor también pueda pasar de ser vendedor de salón a especialista y viceversa, podría plantearse una solución en la cual el vendedor conozca a su categoría y también a su modo de venta, complicando más el armado de un vendedor a cambio una mayor **[flexibilidad](atributos-de-calidad.html)** del modelo.
