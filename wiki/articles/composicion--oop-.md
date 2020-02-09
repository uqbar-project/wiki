---
layout: article
title: Composicion  oop 
---

Supongamos que queremos modelar el comportamiento de personas a la hora de pagar la cuenta después de una comida en un restaurant. Los clientes pagan lo que consumen más la propina, que depende de su humor. Sabemos que la gente feliz deja de propina 25% de lo que salió la comida, la gente enojada no deja nada y los que están de un humor indiferente dejan lo que tienen en el bolsillo.

Queremos que un cliente nos pueda decir cuánto paga en total (propina + lo que consumió) dado el importe de la comida consumida, y además debe ser posible para una persona cambiar de humor a lo largo de la ejecución del programa.

Sería posible resolver toda esta lógica (y la que esté por venir más adelante) con muchos ifs en el cliente, pero es posible modelarlo de otra forma: los diferentes humores del cliente podrían ser otros objetos separados que le ayuden a saber cuánta propina poner, y por su puesto ser polimórficos para que el cliente pueda delegar en ellos esta funcionalidad sin importar cuál sea su humor actual (objeto al cual referencia con algún atributo propio, como ser humor).

#### Wollok

```wollok
class Cliente {
  var property humor
  method cuantoPaga(importeTotal){
    return importeTotal + self.cuantoDePropina(importeTotal)
  }
  method cuantoDePropina(importeTotal){ 
    return humor.cuantoDePropina(importeTotal)
  }
}
  
class Feliz {
  method cuantoDePropina(importeTotal){
    return importeTotal * 1.25
  }

class Enojado {
  method cuantoDePropina(importeTotal){
    return 0
  }
}

class Indiferente {
  var property plataDelBolsillo
  method cuantoDePropina(importeTotal)
    return plataDelBolsillo
  }
}
```

**Nota:** en este ejemplo se ubicó la variable `plataDelBolsillo` en la estrategia Indiferente. Esto implica que cada vez que el cliente cambie de humor a indiferente, hay que indicarle cuánta plata en el bolsillo tiene.

Otra opción podría haber sido poner la `plataDelBolsillo` en el cliente y para que la estrategia Indiferente resuelva cuánto tiene que devolver al recibir el mensaje `cuantoDePropina` hay dos opciones:

- Que la instancia del objeto Indiferente conozca al Cliente y le pida su `plataDelBolsillo`
- Que el cliente se pase por parámetro al pedirle a la estrategia cuánta propina pone, modificando el método para recibir dos parámetros, por ejemplo:

#### Wollok

```wollok
class Cliente {
  method cuantoDePropina(importeTotal){
    return humor.cuantoDePropinaPara(importeTotal,self)
  }
}
```

Ante la necesidad de poder cambiar el humor de la persona, separamos a la Persona (que intuitivamente iba a ser un concepto entero abarcando a su estado de humor) de su Humor en un concepto aparte. Los objetos Humor deben ser polimórficos para la persona, ya que debo poder intercambiar los distintos humores y la persona debería hablarle de la misma forma a cualquiera.

Entonces en vez de tener un objeto que resuelve todo el problema tenemos un objeto que conoce a otros objetos polimórficos para resolver el problema mediante la colaboración. Con esta solución, el flujo del programa ya no se encuentra definido por los ifs y objetos básicos sino por la configuración del cliente y el uso de [polimorfismo](polimorfismo.html).

Es importante notar que **no sería válido modelar una solución a este problema basada en herencia** teniendo personas felices, indiferentes y enojadas, ya que una vez que la persona es instanciada como feliz no es posible cambiarla a indiferente o enojada, ya que implicaría cambiar su clase que **no se puede hacer**.

Entonces, la composición en objetos es simplemente una relación de conocimiento entre dos objetos (por ejemplo, el cliente conoce a su humor) donde el objeto conocido puede cambiarse por otro que sea polimórfico para el que los conoce.

Otro ejemplo podría ser el de las colecciones con un algoritmo de ordenamiento elegido por el usuario ([SortedCollection](sabores-de-colecciones.html) en Smalltalk), donde la colección delega en otro objeto que modela el algoritmo de ordenamiento a usar sobre sus elementos.

Cambiando herencia por composición
----------------------------------

El uso de composición en ocasiones es una solución muy elegante para problemas aparejados por el concepto de [Herencia](herencia.html), que pueden verse en el siguiente ejemplo tomado de un final de Paradigmas de Programación:

> El siguiente texto representa parte del relevamiento realizado en una cadena de venta de electrodomésticos: “Los vendedores pueden ser especialistas o de salón. Los especialistas atienden detrás de mostrador y cobran un premio de 100 pesos por cada venta mayor a 500 pesos. Los vendedores de salón cobran un premio (diferente para cada vendedor) si hacen más de 50 ventas "
>
> Avanzando en el relevamiento, nos dicen lo siguiente:
>
> "Para motivar las ventas en el equipo, decidimos incorporar un cambio: categorías senior y junior. Un vendedor senior tendrá a cargo a un junior. Un vendedor senior recibe como parte del premio un adicional correspondiente al 3% de la las ventas realizadas por la persona que tiene a cargo. Un Junior tiene un porcentaje de descuento en su premio, diferente para cada uno. Por otra parte, si un vendedor junior hace bien las cosas, con el tiempo puede pasar a ser senior" 

La codificación propuesta en el enunciado es:

#### Wollok

```wollok
class VendedorEspecialista {
  const ventas = []
  method premio(){
    return 100
  }
}

class VendedorSalon {
  const ventas = []
  var premio
  method premio(){
    return premio
  }
}

class VendedorSalonSenior inherits VendedorSalon {
  var junior
  method premio(){
    return super() + self.adicionalJunior()
  }
  method adicionalJunior(){
    return junior.totalVentas() * 0.03
  }
}

class VendedorSalonJunior inherits VendedorSalon {
  var descuento
  method totalVentas(){
    return ventas.sum({ venta => venta.monto() })
  }
  method premio(){
    return super() * (1 - self.descuento())
  }
}

class VendedorEspecialistaSenior inherits VendedorEspecialista {
  var junior
  method premio(){
    return super() + self.adicionalJunior()
  }
  method adicionalJunior(){
    return junior.totalVentas() * 0.03
  }
}

class VendedorEspecialistaJunior inherits VendedorEspecialista {
  var descuento
  method totalVentas(){
    return ventas.sum({ venta => venta.monto() })
  }
  method premio(){
    return super() * (1 - self.descuento())
  }
}
```

La solución propuesta tiene problemas que surgen por el mal uso de herencia. Los que podemos destacar son:

- **Repetición de código:** La forma de subclasificar a los vendedores tanto por tipo de vendedor (Salón o Especialista) como por categoría (Senior o Junior) hace que tengamos código repetido entre las hojas del árbol de herencia. Esto tiene problemas, sobre todo si el sistema sigue creciendo de esta forma, ya que la repetición de código es exponencial y realizar un cambio en la lógica del premio de los juniors por ejemplo se propagaría para todos los tipos de vendedores habidos y por haber (tiene problemas de **[extensibilidad](atributos-de-calidad.html)**).
- **Problemas con la [identidad](igual-o-identico-----vs---.html) de los objetos:** El enunciado indica que un junior puede volverse senior con el tiempo, pero el modelo que tenemos no soporta este tipo de cambio en tiempo de ejecución. Un objeto de la clase VendedorSalonJunior no puede cambiar de clase a VendedorSalonSenior, su clase es algo que se mantiene durante toda la vida del objeto. Si tratamos de emular el cambio de clase creando un nuevo objeto y copiando los valores de sus atributos según corresponda lograremos tener el comportamiento de senior pero ya no será el mismo objeto para el sistema. En OOP una de las características de un objeto es su identidad, la cual estaríamos perdiendo si tomamos esa decisión y el problema asociado a este cambio es que todos los objetos que tengan una referencia a nuestro vendedor promovido deberán enterarse de este cambio (y seguramente no lo hagan) para referenciar al nuevo objeto que lo reemplaza. La consecuencia de esto es o bien una complejidad espantosa para mantener las referencias o un sistema **[inconsistente](atributos-de-calidad.html)**.

**¿Cómo se soluciona este problema?** Si cambiamos el modelo para que la categoría (Junior o Senior) sea un objeto aparte que el vendedor conozca y delegamos en este objeto todo aquello que corresponda a ser senior o junior solucionamos ambos problemas a la vez, ya que el valor de las referencias sí puede ser cambiado en tiempo de ejecución, es sólo settear un atributo. Veamos cómo queda la nueva solución:

#### Wollok

```wollok
class Vendedor {
  const ventas = []
  var categoria
  
  method premio(){
    return categoria.premioPara(self)
  }
  method totalVentas(){
    return ventas.sum({ venta => venta.monto() })
  }
}

class VendedorEspecialista inherits Vendedor {
  method premioBase(){
    return 100
  }
}

class VendedorSalon inherits Vendedor {
  var premioBase
  method premioBase(){
    return premioBase
  }
}

class Senior {
  var junior
  method premioPara(unVendedor{
    return unVendedor.premioBase() + self.adicionalJuniorPara(unVendedor)
  }
  method adicionalJuniorPara(unVendedor){
    return junior.totalVentas() * 0.03.
  }
}

class Junior {
  var descuento
  method premioPara(unVendedor){
    return unVendedor.premioBase() * (1 - self.descuento())
  }
}
```

**Disclaimer:** el método `totalVentas` se termina definiendo en la clase Vendedor porque tenía sentido para todos en este dominio, no sólo para los juniors. Para que fuera totalmente análoga podríamos tenerlo definido en Junior y delegar en la categoría, pero siendo que no es inválido que los vendedores lo definan, era más sencillo resolverlo de esta forma. En caso de dudas de dominio, siempre vale preguntar.

Como se puede ver en el diagrama de clases de la solución con composición, para crear un vendedor ya no alcanza sólo con elegir la clase del tipo de vendedor que queremos e instanciarla, sino que tenemos que instanciar dos objetos (al vendedor que queramos y su categoría) y hacer que el vendedor conozca a su categoría, lo cual agrega una **[complejidad](atributos-de-calidad.html)** extra para la creación de nuestros objetos. Si más adelante quisiéramos que un vendedor también pueda pasar de ser vendedor de salón a especialista y viceversa, podría plantearse una solución en la cual el vendedor conozca a su categoría y también a su modo de venta, complicando más el armado de un vendedor a cambio una mayor **[flexibilidad](atributos-de-calidad.html)** del modelo.

A modo de resumen rápido:

**Herencia**

- Estática (no puedo cambiar la clase de un objeto en run-time, si creo otro objeto se pierde la identidad lo cual trae problemas)
- Menos objetos -&gt; Menos complejidad
- Es una relación entre clases! Es mucho más fuerte que la relación entre instancias planteada en composición. La herencia implica no sólo heredar código sino conceptos.

**Composición**

- Dinámico (la implementación se puede cambiar en run-time, ya que se basa sólo en un atributo que se puede settear en cualquier momento con otro objeto que sea polimórfico)
- Aumenta la cantidad de objetos -&gt; Mayor complejidad (Es más complicado entender el todo y hay que configurar adecuadamente las relaciones entre los objetos)
- Se reparten mejor las responsabilidades en objetos más chicos

