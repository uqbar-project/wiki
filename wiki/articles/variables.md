---
layout: article
title: Variables
---

# Referencias

En objetos una variable es una **referencia** a un objeto. La mayoría de las referencias pueden ser reapuntadas a otros objetos mediante la operación de [asignación](transparencia-referencial--efecto-de-lado-y-asignacion-destructiva.html). Al hacerlo eso no modifica al objeto previamente referenciado.

Hay distintos tipos de referencias, dependiendo del contexto en el cual son declaradas:

- atributos: se usan para que el objeto mantenga un estado propio.
- locales: se declaran dentro de un método, sólo son visibles desde el mismo, y no sobreviven a su ejecución.
- parámetros: forman parte de la firma del método, no pueden ser reapuntadas a otros objetos dentro del método.

## Asignación de variables

La asignación de variables se logra de la siguiente forma:

```scala
variable = expresion-que-devuelve-un-objeto
```

y debe interpretarse que cuando se evalúa esta línea, la variable referencia al objeto resultado de la expresión de la derecha.

Entonces, al asignar una variable **no estoy creando ningún objeto** ni estoy cambiando al objeto referenciado anteriormente por dicha variable, sólo se cambia cuál es el objeto al que está apuntando esa referencia.

### var y const
En Wollok, las referencias pueden declararse como variables (con la palabra reservada `var`) o como constantes (con la palabra reservada `const`). Las constantes están pensadas para ser usadas siempre que no se espere que la referencia pueda cambiar de valor, con lo cual intentar asignar una constante luego de su inicialización no está permitido.

```scala
var edad = 15
const iva = 21
edad = 16 // esto anda perfecto
iva = 19 // esto tira error en tiempo de compilación
```

Error común: ¿qué significa "cambiar un objeto"? 
- ¿Lo cambio **por otro**?
- ¿O le cambio **sus atributos**?

Miremos el siguiente ejemplo:
```scala
const laPreferida = pepita // atentos al const
laPreferida.volar() // ¿Qué sucede en este caso? ¿Da error? 
laPreferida = pepona // ¿Y en este caso?
```

`laPreferida.volar()` no da error. Esto es porque al enviar el mensaje `volar()`, `laPreferida` (que apunta al mismo objeto que `pepita`) está cambiando su **estado interno**, su energía. ¡Pero la **flecha** `laPreferida` no se modifica! Se modifica sólo la energía de pepita al volar.

En cambio, en `laPreferida = pepona` sí hay un error de compilación. Estoy intentando modificar a dónde apunta `laPreferida`, pero como esa referencia es `const` nunca podrá dejar de apuntar a pepita.

Es importante entender que `const laPreferida` **no significa que el objeto no pueda modificar su estado interno, sino que no puedo hacer que la flecha laPreferida apunte a otro objeto**.

## Inicialización

Un problema común que suele darse con atributos de los objetos es olvidarse de inicializarlos.

Lo que sucede cuando una variable no ha sido inicializada puede variar de un lenguaje a otro: algunos (por ejemplo Smalltalk) tienen un objeto especial para representar la nada que entienden muy poquitos mensajes, otros (como Java o Wollok) directamente usan un valor primitivo. Pero lo importante es que para poder usar una variable de forma razonable, la misma debería referenciar a un objeto que entienda los mensajes que esperamos mandarle, de lo contrario se generará un error.

¿Cuál es el momento adecuado para inicializar un atributo? Si bien podrían darse situaciones en las cuales se quiera o necesite postergar la inicialización de un atributo, lo más común es querer inicializar los atributos al momento de la creación del objeto, de esa forma nunca se llegará a un estado en el cual el se le mande un mensaje al objeto y el mismo falle porque no se haya inicializado un atributo previamente.

En Wollok podemos inicializar las variables al momento de declararlas. Eso en general es suficiente para trabajar con objetos bien conocidos (como en el ejemplo que se muestra más adelante). En caso de trabajar con objetos instanciados a partir de [clases](clases.html), es posible inicializar los atributos con valores distintos para cada instancia usando las [herramientas de instanciación](herramientas-de-instanciacion.html) disponibles (en otros lenguajes en los cuales las clases son objetos, como Smalltalk, esto se logra mediante mensajes a las clases).

## Ejemplo completo

Dado el siguiente código Wollok:

```scala
object pepita {
  var energia = 100
  
  method energia(cantidad) {
    energia = cantidad
  }
}
```

... y luego le mando el mensaje...

```scala
pepita.energia(50)
```

Cuando el objeto se crea, la variable energía se inicializa apuntando al objeto 100. Al mandar el mensaje para settearle la energía, el atributo energia que tiene pepita pasa de apuntar a 100 a apuntar a 50. O sea que sólo cambia a quién conoce pepita mediante la referencia energia.

SIEMPRE lo que se encuentre a la izquierda de la asignación debe ser una variable, no se puede asignar un objeto (y por el mismo motivo no se puede asignar un envío de mensajes). Las siguientes expresiones son inválidas:

```scala
3 = 5                 // <--- 3 es un objeto, no una referencia!!!
pepita.energia() = 10 // <--- pepita.energia() es un envío de mensajes, no una referencia!!!
```

En ningún caso vamos a poder modificar desde fuera del objeto que tiene una referencia el valor de la misma, siempre hay que mandarle un mensaje a ese objeto para que la cambie. Esto está relacionado con la idea de [encapsulamiento](encapsulamiento.html), que es una de las bases del paradigma de objetos.

# Atributos: errores comunes

Los atributos son obviamente muy útiles, ya que permiten que un objeto recuerde toda la información que necesita para poder usarla en cualquier momento. Sin embargo, hay que ser criteriosos respecto a cuándo usarlos, ya que pueden ser el motivo de inconsistencias y dificultad para mantener nuestros programas.

Los siguientes son los algunos errores comunes de ver respecto al uso de atributos:

- Atributos redundantes: si cierta pieza de información puede ser calculada a partir de otra, no uses un atributo que tenga que ser mantenido consistente, usá un método que calcule lo que necesitás a partir de la otra información disponible. Para más información podés leer [este artículo](oo-temporary-variable.html)
- Atributos innecesarios: no hay que perder de vista que nuestro programa es un **modelo**, y por ende no hace falta que el objeto recuerde información que luego no va a ser usada.
- Uso de atributos en vez de locales: si necesitamos recordar cierta información sólo dentro de la ejecución de un método, lo correcto es usar una local, no un atributo.

Como regla general para evitar estos problemas, preguntate: ¿hay alguna forma de no usar un atributo para lo que estás haciendo? si la hay, ***no uses un atributo***.

¿Y las referencias circulares? Es perfectamente válido que dos objetos se conozcan entre ellos, en el caso de que sea necesario obviamente, las reglas anteriores valen también para esto. El único recaudo que hay que tener es asegurar la consistencia del estado de ambos objetos, lo cual puede simplificarse asegurando que esas referencias se modifiquen dentro de una misma operación, y no de forma independiente.
