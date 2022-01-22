---
layout: article
title: Guia rapida de Kotlin
categories: [kotlin, lenguaje, guia, referencia]
featured: true
---

La siguiente es una guía de _syntactic sugars_ de Kotlin, algunos de los cuales trabajan conceptos más profundos que veremos a lo largo de la materia.

# Definición de una clase

Una clase necesita un nombre, atributos a los cuales referencia y métodos, definidos mediante el prefijo `fun`.

```kotlin
val ENERGIA_MINIMA = 10

class Ave {
    var energia = 0
    fun volar() { energia = energia - 10 }
    fun comer(cuanto: Int) { energia = energia + (cuanto * 2) }
    fun esFeliz() = energia > ENERGIA_MINIMA
    fun resetearEnergia() { energia = 0 }
}
```

## Reglas generales para la clase

* podemos escribir múltiples clases en un archivo Kotlin
* la definición de la clase se encierra entre llaves

## Atributos

* la variable ENERGIA_MINIMA se define como una constante y es referenciable dentro de cualquier clase que esté dentro de ese archivo. Otra variante es definir atributos asociados a una clase específica (ver companion object), suele usarse como constantes o valores que difícilmente cambien.
* la variable energia **es una variable de instancia** porque cada objeto Ave tiene su propio valor.
* las variables tienen un tipo que se infiere en base al valor: en el caso de la energia es un número (`Int`) porque se asocia al valor `0` aunque podemos explicitarla nosotros de la siguiente manera: 

```kotlin
var energia: Int = 0
// la variable energia 
//    tiene el tipo Int y el valor por defecto 0
```

* Kotlin automáticamente define getters y setters para la variable `energia` (no es necesario hacer nada, mientras no especifiquemos la visibilidad del atributo a privada, de la siguiente manera: `private var energia = 0`)
* La manera de invocar al getter es: `objeto.atributo` y la manera de invocar al setter es `objeto.atributo = valor`:

```kotlin
pepita.energia = 100    // <-- equivale a pepita.setEnergia(100)
pepita.energia          // <-- equivale a pepita.getEnergia()
```

¡Ojo! si bien parece que estamos accediendo diréctamente a la variable de instancia, no es así. Xtend simplemente traduce esa sintaxis a la anterior. Es decir que en ambos casos estamos igualmente llamando al getter y al setter. Pueden probar definiendo la variable `energia` como privada y el IDE mostrará un mensaje de error "Cannot access 'energia': it is private in 'Ave'".


## Métodos

* respecto a los métodos, algunos producen efecto (volar y comer) y otros simplemente devuelven un valor (esFeliz). 
* en el caso de los métodos con efecto, se delimitan con llaves. Por defecto los métodos que no devuelven nada no tienen ninguna anotación de tipo, se dice que son `void` o `Unit`.

```kotlin
fun volar() { energia = energia - 10 }
```

* los métodos que solo devuelven valores y tienen una sola línea se definen con el símbolo `=`:

```kotlin
fun esFeliz() = energia > ENERGIA_MINIMA
```

* también es posible definir un método que devuelve un valor mediante las llaves, definiendo una anotación de tipo para el método:

```kotlin
fun esFeliz(): Boolean { 
    return energia > ENERGIA_MINIMA
}
```

En este caso el tipo de retorno del método es Boolean. Si el método tiene varias líneas es necesario utilizar este formato en lugar del `=`.

# Referencias variables y valores

En Kotlin, al igual que muchos otros lenguajes, se diferencian las referencias como

* **Variables**: son referencias que pueden inicializarse apuntando a un objeto, y luego reasignarse a otro:

```kotlin
var unString = "Pepito"
unString = "Otro String"
```

* **Constantes**: son referencias que nacen apuntando a un valor y no pueden ser modificadas para apuntar a otro objeto. Serían como "constantes".

```kotlin
val constante = "Constante"
constante = "Otro"  // <----- NO COMPILA !
```

¡Ojo! no confundir el hecho de que no se pueda modificar la "referencia" de la mutabilidad/inmutabilidad del objeto al que apunta. Puedo tener un "val" apuntando a un elemento que sí mute.

```kotlin
val perro = Perro()
perro.nombre("Juan")
perro = Perro()        // <----- NO COMPILA: no puedo modificar la referencia
perro.nombre("Carlos") // <---- SI COMPILA y puedo mutar la referencia nombre de perro
```

## Cuándo debería usar val y cuándo var

Por defecto definí tus variables como `val`, a menos de que necesites modificar las referencias. _Por ejemplo_: la edad de una persona debería poder modificarse, en cuanto al nombre puede ser que no necesites modificarlo o sí, eso dependerá de las reglas de negocio. El motivo principal es acotar el efecto en nuestros programas, **mientras menor sea el efecto, más fácil es controlar nuestro software, y más fácil será testearlo**.

# Objetos singleton

Kotlin provee la capacidad de definir objetos:

```kotlin
object Pepita {
    var energia = 100
    fun volar(minutos: Int) {
        energia -= minutos * 2 + 10
    }
    fun comer(gramos: Int) {
        energia += gramos * 4
    }
}

fun main() {
    Pepita.energia = 150
    Pepita.volar(5)
    Pepita.comer(2)
    System.out.println("La energia de pepita es ${Pepita.energia}")  // "La energia de pepita es 88"
}
```

Pepita es una instancia que se puede acceder globalmente, representa una implementación _thread safe_ del **Singleton** que es más trabajosa de implementar en Java (podés investigar más en [este artículo](https://devexperto.com/object-kotlin-singleton/)). Si trabajaste en **Wollok** (o **Scala**) el concepto es exactamente similar, solo que el nombre debe comenzar con mayúscula.

# Tipos de datos

## Strings

Un string se encierra entre dobles comillas, o bien podemos aprovechar para escribir un texto largo con triples comillas dobles (lo que nos permite incluso utilizar enters). Podemos interpolar referencias de Kotlin mediante `$` o bien utilizar código ejecutable usando `${zzz}` donde zzz es código Kotlin.

```kotlin
class Cliente {
    var nombre = "Juan" // string simple

    fun saludo() = "Hola $nombre" // string simple interpolando una referencia
    
    // string con múltiples líneas interpolando código Kotlin
    fun saludoFormal() =
        """
        Bienvenido, ${nombre.trim()} a nuestra aplicación.
        En breve nos contactaremos con ud.
        """
}
```

## Números

Existen muchos tipos de datos diferentes para números:

* **Int**: es un número entero que admite negativos pero sin decimales
* **Double**, **Float**: son números reales que admiten decimales pero con errores en las operaciones, es por ello que no debemos usarlo para operaciones sensibles (como transacciones bancarias o que requieran cálculos exactos). ¿Por qué? Por este código que podés probar en [este REPL](https://play.kotlinlang.org/)

```kotlin
fun main() {
    val a: Double = 0.02
    val b: Double = 0.03
    val c: Double = b - a
    System.out.println(c)  // 0.009999999999999998
}
```

* **BigDecimal**: es el tipo de dato que conviene utilizar ya que no produce errores de redondeo (permite trabajar con una cantidad exacta de decimales y truncarlos o redondearlos en caso de ser necesario)

```kotlin
import java.math.BigDecimal

fun main() {
    val a: BigDecimal = BigDecimal("0.02")
    val b: BigDecimal = BigDecimal("0.03")
    val c: BigDecimal = b - a
    System.out.println(c)    // 0.01
}
```

Tanto Int, como Double como BigDecimal representan objetos a los que podés enviarle mensajes:

```kotlin
fun main() {
    val numero: Double = 10.0
    System.out.println(numero.inc())   // 11.0
    System.out.println(numero.rem(3))  // 1.0
}
```

Para más información pueden ver [esta página](https://kotlinlang.org/docs/basic-types.htm).

## Colecciones mutables e inmutables

En Kotlin, todas las colecciones vienen en dos "sabores": mutables e inmutables. Las primeras soportan modificar sus elementos (agregar, quitar, actualizar), mientras que las segundas solo permiten acceder a sus elementos. Queda a criterio de quien programa cuál utilizar en cada caso, prefiriendo desde este espacio las inmutables (porque algo que no se puede modificar es menos propenso a errores).

Existen literales para definir listas, conjuntos y mapas (dictionaries):

```kotlin
fun main() {
    // Lista inmutable
    val myList = listOf("Hello", "World")
    myList.size
    // ERROR, no puedo agregar un elemento a una lista inmutable
    // └ myList.add("Goodbye")
    
    // Lista mutable
    val myMutableList = mutableListOf("Hello", "World")
    myMutableList.add("Goodbye")
    System.out.println("${myMutableList[1]}")  // "World"

    // Set inmutable
    val mySet = setOf("Hello", "World")
    // ERROR, no puedo agregar un elemento a un set inmutable
    // └ mySet.add("Goodbye")

    // Set mutable
    val myMutableSet = mutableSetOf("Hello", "World")
    myMutableSet.add("Goodbye")
    myMutableSet.add("Hello")  // no tiene efecto porque ya hay un elemento "Hello"
    System.out.println("${myMutableSet.size}")  // 3

    // Mapa/Diccionario inmutable
    val myMap = mapOf("a" to 1 , "b" to 2)
    // ERROR, no puedo agregar un elemento a un set inmutable
    // └ myMap.set("c", 3)
    
    val myMutableMap = mutableMapOf("a" to 1 , "b" to 2)
    myMutableMap.set("c", 3)
    System.out.println("${myMutableMap.size}")  // 3
}
```

Recordemos que

* **listas**: respetan el orden en el que se agregan (como una fila) y admiten duplicados.
* **conjuntos**: no tienen orden y tampoco admiten duplicados. Dos objetos son iguales en base a la definición de equals() y hashCode().
* **mapas**: son un conjunto de pares clave/valor. Se acceden por clave.

> Ojo 👀: no hay que mezclar las ideas de `val` y `var` con la (in)mutabilidad de las colecciones. Por ejemplo, una colección inmutable podría estar referenciada con var, mientras que una mutable podría ser val.

## Rangos con arrays

Es posible generar un rango de números:

```kotlin
// Array de enteros con valores [0, 0, 0, 0, 0]
val arrZeros = IntArray(5)

// Array de enteros de tamaño 5 con valores [42, 42, 42, 42, 42]
val arrConstants = IntArray(5) { 42 }

// Podemos utilizar una lambda para inicializar un array: [0, 1, 2, 3, 4]
var arrLambda = IntArray(5) { it }
// ... o [1, 2, 3, 4, 5]
var arrLambda = IntArray(5) { it + 1 }
```

Más abajo explicamos definición de bloques o lambdas.

## Inferencia de tipos

Kotlin cuenta con inferencia de tipos, lo que permite

* que exista chequeo de tipos
* pero que muchas veces no sea necesario definir los tipos de las expresiones

Vemos un ejemplo en vivo, mostrando cómo cambia la solapa "Structure" (disponible mediante `Alt`+ `7`) cuando modificamos el código:

![Kotlin Type Inference](/img/wiki/kotlin-typeInference.gif)

Volviendo a la inferencia de tipos, es fundamental poder contar con un lenguaje que tenga chequeo de tipos para detectar errores en forma temprana pero **que no me obligue a definir los tipos todo el tiempo**. La definición de tipos es obligatoria cuando la definición pueda resultar ambigua para Kotlin, por ejemplo cuando definas un método que retorna un valor pero no lo anotes en la definición:

```kotlin
fun resetearEnergia() {
    energia = 0
    return true   // ERROR: la definición del método conflictúa con este return
}
```

En ese caso el IDE te mostrará un error y lo podés solucionar fácilmente indicando el tipo del valor a retornar (o bien eliminando la instrucción `return`):

![Kotlin Fix method return](/img/wiki/kotlin-fixMethodReturn.gif)


# Instanciación y constructores

## Instanciación por defecto

Para instanciar un objeto, Kotlin no utiliza la palabra `new`, simplemente se invoca mediante el nombre de la clase y paréntesis:

```kotlin
class Entrenador {
    val ave = Ave()
```

## Definiendo constructores

Adicionalmente, podemos definir parámetros en la construcción de una clase (lo que en otros lenguajes se conoce como constructor):

```kotlin
class Ave(var energia: Int = 0) {
    ...
}
```

El valor por defecto indica que podemos crear un ave sin pasar parámetros, en cuyo caso el valor de su energía será 0:

```kotlin
val pepita = Ave() // un ave con energia = 0
```

Pero también podemos pasar un valor:

```kotlin
val pepita = Ave(energia = 150) // un ave con energia = 150
```

Si en cambio no definimos un valor por defecto para energia

```kotlin
class Ave(var energia: Int) {
    ...
```

es obligatorio pasarle un valor para energía:

```kotlin
val ave = Ave()              // ERROR: No value passed for parameter 'energia'
val ave = Ave(energia = 200) // OK
```

## Constructores secundarios

Por lo general solo es necesario definir un constructor por defecto, pero en caso de que lo necesites te dejamos [este artículo que explica cómo escribirlos](https://kotlinlang.org/docs/classes.html#secondary-constructors).


# Herencia y redefinición de métodos

A continuación vemos cómo definir dos subclases de Ave: Golondrina y Torcaza.

![image](/img/languages/xtendInheritanceShort.gif)

```scala
@Accessors
class Ave {
    int energia = 0
    static int ENERGIA_MINIMA = 10
    def volar() { energia = energia - 10 }
    def comer(int cuanto) { energia = energia + (cuanto * 2) }
    def esFeliz() { energia > ENERGIA_MINIMA }
}

class Golondrina extends Ave {
    override esFeliz() { true }
}

class Torcaza extends Ave {
    int vecesQueVolo = 0

    override volar() {
        super.volar()
        vecesQueVolo++
    }
}
```

Aquí vemos que

* Golondrina y Torcaza heredan de Ave, indicado mediante la palabra clave `extends`
* Golondrina **redefine** el comportamiento de esFeliz, lo pisa, y esto requiere la palabra clave `override` (no funciona si intentamos definirlo con `def`)
* Torcaza también lo redefine, pero fuerza a llamar al comportamiento de la superclase mediante la palabra clave `super`, indicando luego el mensaje a aplicar. Como regla general solo deben utilizar `super` cuando no puedan utilizar `self`, como en este caso (entrarían en loop infinito)

# Clases y métodos abstractos

Podemos definir a Ave como clase abstracta, esto producirá que no podamos instanciar objetos Ave. Una clase abstracta puede definir solo la interfaz de un método, lo que se conoce como método abstracto. Veamos el siguiente ejemplo:

![image](/img/languages/xtendAbstractClassesAndMethods.gif)

En el ejemplo:

* primero definimos Ave como abstracta
* eso provoca que el compilador Xtend tire un error cuando queremos instanciar un Ave en la clase Ornitologo
* lo corregimos instanciando una Golondrina
* luego, queremos definir un método abstracto: esFeliz. Para ello reemplazamos la definición por una cáscara que solo dice que esFeliz debe devolver un booleano. Dado que no hay código Xtend nos fuerza a definir el tipo de retorno del método (y de sus parámetros) porque no puede inferirlo.
* todos los métodos abstractos deben estar implementados en las subclases: el compilador nos avisa que falta la definición de esFeliz() en Torcaza. Con un botón derecho "Add unimplemented methods" pegamos la definición copiada.
* como nos falta la constante que estaba en Ave, la bajamos mediante `Alt` + `Flecha abajo`

y finalmente todo compila.

# Constructores

Un constructor se define con la palabra reservada `new` (equivalente al `constructor` de Wollok):

```scala
@Accessors
class Golondrina {
    int energia
    new() {
        this(100)   // llama al constructor con parámetros
        // para llamar al constructor de la superclase es necesario utilizar super(params)
    }
    new(int energia) {
        this.energia = energia
    }
}
```

* Por defecto, si no hay constructores se genera uno por defecto sin parámetros y no hace falta definirlo
* Es posible definir múltiples constructores, como vemos en el ejemplo, con diferente cantidad de parámetros o de tipos
* Cuando definimos constructores sobre una clase, se pierde el constructor por defecto
* **Los constructores de Xtend no se heredan**

# Bloques

Un bloque permite definir una porción de código, también llamada expresión lambda:

```scala
val cuadrado = [ int num | num ** 2 ]
cuadrado.apply(5)
```

De esta manera podemos enviar bloques como parámetros, algo muy útil para trabajar entre otras cosas con las colecciones (`map`, `filter`, `fold`, etc.)

La sintaxis general es

```scala
[ | ... ]                // bloque sin parámetros
[ elem | ... ]           // bloque con un parámetro
[ int a, int b | a + b ] // bloque con dos parámetros
```

## Variable implícita it

De la misma manera que cuando estamos dentro de una clase, podemos acceder a una variable de instancia con `this`

```scala
this.energia
```

o sin él:

```scala
energia
```

también podemos usar una variable implícita `it` dentro de un método.

```scala
val it = new Ave()
volar       // equivale a it.volar()
comer(2)    // equivale a it.comer(2)
```

Dentro de una expresión lambda, `it` es la variable implícita del primer parámetro, por lo tanto todas estas expresiones son equivalentes:

```scala
alumnos.filter [ alumno | alumno.estudioso() ]
alumnos.filter [ it | it.estudioso() ]
alumnos.filter [ it.estudioso() ]
alumnos.filter [ it.estudioso ]
alumnos.filter [ estudioso ]
```

![image](/img/languages/xtendItImplicitVariable.png)

# Manejo de nulls

Los valores nulos son siempre un dolor de cabeza, Xtend tiene algunos trucos para facilitar un poco más el trabajo con ellos.

## Elvis operator

Parece un emoticón, pero `?:` es un shortcut para utilizar un valor por defecto cuando una expresión pueda ser nula:

```scala
val nombre = person.firstName ?: 'You'
```

Si la expresión que está a la izquierda se evalúa como null, `nombre` se asigna a la segunda expresión.

## Null safe operator

También podemos resolver envíos de mensajes a referencias que potencialmente podrían ser nulas:

```scala
val mejorAlumno = alumnos.find [ ... ]
...
mejorAlumno?.felicitar()
```

En este caso, el operador `?.` es equivalente a preguntar `if (mejorAlumno) mejorAlumno.felicitar()`

## Comparar referencias

Después de varios cambios, Xtend dejó las cosas como la mayoría de los lenguajes. Tenemos dos formas de comparar referencias:

```scala
ref1 == ref2     // compara por igualdad, esto significa que son iguales si son las referencias
                 // apuntan al mismo objeto o bien, en base a la definición del método equals
                 // en la clase ref1 (sabiendo que ref1 no es nulo)
                 // en la clase asociada a ref1
ref1 === ref2    // compara por identidad, esto significa que son iguales si las referencias
                 // apuntan al mismo objeto en memoria, determinado por la VM y no se puede cambiar
```

**Tener especial atención a los strings**, ya que dos strings con el mismo contenido pueden ser iguales pero no idénticos, dependiendo de las estrategias de optimización de la VM. Siempre es conveniente utilizar ==, que además se puede modificar.

# Métodos avanzados

## Obligatoriedad del return en métodos

Por lo general, los métodos devuelven la última expresión que contienen. Pero a veces es necesario cortar el flujo de envío de mensajes, como por ejemplo aquí:

```scala
def gradoDeFelicidad() {
    if (!esFeliz) {
       return 0
    }
    ... cálculo complejo ...
}
```

Para determinar el grado de felicidad de alguien, tenemos como precondición que sea feliz. Y para simplificar la definición, escribimos el `if` y forzamos el return (dado que escribir únicamente `0` no tendrá efecto, porque Xtend seguirá evaluando el resto de las expresiones hasta terminar la última y nosotros queremos justamente cortar el flujo).

En el caso de un método que solo busque producir un efecto (`void`), es necesario utilizar `return;` con punto y coma...

```scala
def metodoConEfecto() {
    ... cambios ...
    if (!situacion) {
       return;
    }
    ... otros cambios ...
}
```

Igualmente, siempre es preferible tratar de extraer métodos más pequeños para simplificar la lógica.

## Extension methods

Una de las herramientas más poderosas consiste en definir **extension methods**. Supongamos que un negocio tiene un horario de apertura y de cierre y queremos saber, dada una hora, si está abierto.

```scala
class Negocio {
    int horarioApertura
    int horarioCierre

    def estaAbierto(int horaActual) {
        horaActual.between(horarioApertura, horarioCierre)
    }
}
```

Por supuesto, no compila. No existe el método between asociado a los enteros. Pero en otro archivo vamos a definir un método estático (no asociado a un objeto):

```scala
class NumberUtils {
    static def between(int num, int from, int to) {
        num >= from && num <= to
    }
}
```

Y ahora, desde el archivo Negocio.xtend, vamos a importar el método como `static extension`:

![image](/img/languages/xtendExtensionMethod.gif)

El código a agregar es

```scala
// después de la definición de package
import static extension ar.edu.unsam.prueba.NumberUtils.*
```

Esto produce que automáticamente, el compilador Xtend marque en naranja el método between y nuestra clase Negocio compile perfectamente. En resumen, un extension method permite que nosotros agreguemos comportamiento por afuera de la definición de una clase como si estuviéramos trabajando en ella, algo muy importante cuando la clase no podemos modificarla (como en el caso de Integer , String), o bien cuando se está regenerando todo el tiempo (cuando tenemos un framework que genera código para nosotros), sin contar que además estamos respetando la idea de **mensaje** (y por consiguiente, la posibilidad de seguir trabajando con polimorfismo).

Los métodos `map`, `filter`, `fold`, `length`, `any`, etc. son todos extension methods de Collections.

## Dispatch methods

Xtend permite trabajar con **multimethods**, más adelante tendremos [este ejercicio para contarlo con más profundidad](https://docs.google.com/document/d/1XWq9azqchoJZ7h8-hLcpA1Zj5T1UtvFtDKbpzxoQ-dw/edit?usp=sharing)

# @Data

Para definir un objeto inmutable, debemos:

* generar una clase
* con atributos privados
* y un constructor donde pueda asignar ese estado

Xtend provee la anotación @Data para lograr eso:

```scala
@Data
class Point {
    int x
    int y
}
```

Esto equivale a definir el constructor con dos parámetros (x, y) y los getters para los atributos x e y, sin setters (dado que queremos únicamente representar un valor). Por lo tanto, esta definición compila perfectamente:

```scala
class TestPoint {
    def test() {
        new Point(2, 4).x
    }
}
```

Si por el contrario intentamos asignar el valor de `x`:

```scala
new Point(2, 4).x = 2
```

nos dirá `The field x is not visible`.

# With operator

Otro syntactic sugar muy interesante de Xtend es la posibilidad de enviar múltiples mensajes al mismo objeto, mediante el operador with `=>`, algo muy útil cuando estamos instanciando objetos:

```scala
val ventaNacional = new Venta => [
    cantidadKilos = 12
    fechaVenta = new Date
    parcela = parcela50
    comprador = new CompradorNacional
]
```

De esta manera, todos los mensajes se apuntan al objeto que resulta de evaluar la expresión `new Venta`, y simplifica el envío de mensajes: 

```scala
ventaNacional.cantidadKilos = 12
ventaNacional.fechaVenta = new Date
ventaNacional....
```

## Links relacionados

* [Colecciones](https://docs.google.com/document/d/1lzOStySb8i94oVvZUIxkgymf2tuCDuXzqSTnClPqKSM/edit?usp=sharing)
* [Intro a manejo de errores con excepciones](https://docs.google.com/document/d/1G0a9j-OA0rIEA5cdvEhIMbztJVo86ssvZKBK8HL9akg/edit?usp=sharing)
* [Ejercicio del Monedero](https://docs.google.com/document/d/1vVW91adl0p-NxGNpe8fqmC_5YmBkrxaLDFKyZ0xZb9Y/edit?usp=sharing) para ver cómo interactúan la UI y el dominio a partir de errores del dominio y del sistema
* [Testing](https://docs.google.com/document/d/11mVR-4wEZhlQMDEqrfQeYLypEsrSqXv98dr78SA0Oq4/edit?usp=sharing)
