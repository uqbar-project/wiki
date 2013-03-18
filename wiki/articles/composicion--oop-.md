**Completar con teoría**

Ejemplo de final
----------------

El siguiente texto representa parte del relevamiento realizado en una cadena de venta de electrodomésticos: “Los vendedores pueden ser especialistas o de salón. Los especialistas atienden detrás de mostrador y cobran un premio (todos los especialistas cobran el mismo monto) por cada venta mayor a 500 pesos. Los vendedores de salón cobran un premio (diferente para cada vendedor) si hacen más de 50 ventas "

Avanzando en el relevamiento, nos dicen lo siguiente:

"Para motivar las ventas en el equipo, decidimos incorporar un cambio: categorías senior y junior. Un vendedor senior tendrá a cargo a un junior. Un vendedor senior recibe como parte del premio un adicional correspondiente al 3% de la las ventas realizadas por la persona que tiene a cargo. Un Junior tiene un porcentaje de descuento en su premio, diferente para cada uno. Por otra parte, si un vendedor junior hace bien las cosas, con el tiempo puede pasar a ser senior" "

La codificación propuesta en el enunciado es:

Diagrama de clases: [1](http://yuml.me/0dd23abc)

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

La solución propuesta tiene problemas que surgen por el mal uso de herencia. Los que podemos destacar (a nivel PdeP) son:

-   **Repetición de código:** La forma de subclasificar a los vendedores tanto por tipo de vendedor (Salón o Especialista) como por categoría (Senior o Junior) hace que tengamos código repetido entre las hojas del árbol de herencia. Esto tiene problemas, sobre todo si el sistema sigue creciendo de esta forma, ya que la repetición de código es exponencial y realizar un cambio en la lógica del premio de los juniors por ejemplo se propagaría para todos los tipos de vendedores habidos y por haber.
-   **Problemas con la [identidad](igual-o-identico-----vs---.html) de los objetos:** El enunciado indica que un junior puede volverse senior con el tiempo, pero el modelo que tenemos no soporta este tipo de cambio en tiempo de ejecución. Un objeto de la clase VendedorSalonJunior no puede cambiar de clase a VendedorSalonSenior, su clase es algo que se mantiene durante toda la vida del objeto. Si tratamos de emular el cambio de clase creando un nuevo objeto y copiando los valores de sus atributos según corresponda lograremos tener el comportamiento de senior pero ya no será el mismo objeto para el sistema. En OOP una de las características de un objeto es su identidad, la cual estaríamos perdiendo si tomamos esa decisión y el problema asociado a este cambio es que todos los objetos que tengan una referencia a nuestro vendedor promovido deberán enterarse de este cambio (y seguramente no lo hagan) para referenciar al nuevo objeto que lo reemplaza. La consecuencia de esto es o bien una complejidad espantosa para mantener las referencias o un sistema inconsistente.

**¿Cómo se soluciona este problema?** Si cambiamos el modelo para que la categoría (Junior o Senior) sea un objeto aparte que el vendedor conozca y delegamos en este objeto todo aquello que corresponda a ser senior o junior solucionamos ambos problemas a la vez, ya que el valor de las referencias sí puede ser cambiado en tiempo de ejecución, es sólo settear un atributo. Veamos cómo queda la nueva solución:

Diagrama: [2](http://yuml.me/b266b1e2)

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
