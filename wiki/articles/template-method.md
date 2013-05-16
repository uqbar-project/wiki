Para calcular el precio de un producto, tenemos que considerar

-   todos los productos tienen un precio de venta base
-   los productos importados tienen un cargo extra determinado por el peso (300 $ x cada kilo)
-   a los productos nacionales se les suma un adicional del 10% de impuestos internos
-   y finalmente otros productos especiales incorporan un extra de 5 $ si el producto pesa más de 4 kg.

Si elegimos modelar los productos mediante una jerarquía de subclasificación, tenemos

-   Una superclase Producto, con 3 subclases
    -   ProductoImportado
    -   ProductoNacional
    -   ProductoEspecial

¿Dónde ubicamos la responsabilidad de determinar el precio de un producto?

Pensamos primero en cómo resolver el precio de venta de un producto Importado <code>

`metodo precioVenta() `
`    precioVentaBase + (300 * peso)  `
`fin`

</code>

Y el producto nacional <code>

`metodo precioVenta() `
`    precioVentaBase + precioVentaBase * 0.10`
`fin`

</code>

El **template method** es una técnica que permite agrupar algoritmos similares, donde

-   la superclase define cómo es el algoritmo principal
-   cada subclase define comportamiento específico de una parte de ese algoritmo

Entonces en la clase producto definimos que el precio de venta sale del precio de venta base, y le pedimos a cada subclase que implemente el costo adicional:

<code>

`metodo precioVenta() `
`    precioVentaBase + self costoAdicional`
`fin`

</code>

En lenguajes con chequeo de tipos, Producto debe tener definido un costoAdicional en su interfaz. Esto se puede implementar

-   con un comportamiento default (por ejemplo, haciendo que costoAdicional devuelva 0)
-   o bien con un **método abstracto** (abstract method), que obliga a redefinir el método en las subclases

Vemos la implementación en xtend:

<code>

`def double precioVenta() {`
`    this.precioVentaBase + this.costoAdicional`
`}`

`def double costoAdicional()   // abstract method`

</code>

En lenguajes con tipado dinámico se puede definir un método que explícitamente devuelva error para obligar a las subclases a implementarlo, ya que si ProductoImportado no define un costoAdicional el method lookup va hacia la superclase donde arroja el error. Vemos el ejemplo en Smalltalk:

<code>

`precioVenta`
`     ^precioVentaBase + self costoAdicional`

`costoAdicional`
`     self subclassResponsibility`

</code>
