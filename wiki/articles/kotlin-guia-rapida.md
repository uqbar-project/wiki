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

¡Ojo! si bien parece que estamos accediendo diréctamente a la variable de instancia, no es así. Kotlin simplemente traduce esa sintaxis a la anterior. Es decir que en ambos casos estamos igualmente llamando al getter y al setter. Pueden probar definiendo la variable `energia` como privada y el IDE mostrará un mensaje de error "Cannot access 'energia': it is private in 'Ave'".


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

# Companion object

Kotlin provee la posibilidad de definir un objeto **companion** dentro de una clase, que es global para todas sus instancias:

```kotlin
class Ave {
    companion object {
        var ENERGIA_MINIMA = 100
        fun subirEnergiaMinima(cuanto: Int) { ENERGIA_MINIMA += cuanto }
        fun crear() = Ave()
    }
    var energia = 0
    fun esFeliz() = energia > ENERGIA_MINIMA
    ...
```

- en lugar de definir la referencia `ENERGIA_MINIMA` como constante por fuera de la clase, la asociamos al _companion object_
- para manipular la energía mínima (como por ejemplo para subirla o bajarla en base a un valor), debemos hacerlo también dentro del _companion_
- y también ofrecemos un método para crear un Ave, que por el momento solamente hace `Ave()`, pero el mecanismo de instanciación podría tornarse más complejo y el _companion object_ es adecuado para tal fin.

Todo lo que definimos en el _companion object_ es accesible para atributos y métodos de instancia (como por ejemplo el método `esFeliz`). Desde otra clase, podemos invocar a la función que crea un ave de la siguiente manera:

```kotlin
val ave = Ave.crear()
```

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

    fun otroSaludo() = "Hola ${nombre.uppercase()}" // interpolamos una expresión
    
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

Para más información pueden ver [esta página](https://kotlinlang.org/docs/basic-types.html).

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

Para más información recomendamos leer 

- [el apunte de la materia sobre Colecciones](https://docs.google.com/document/d/1lzOStySb8i94oVvZUIxkgymf2tuCDuXzqSTnClPqKSM/edit#)
- [la página oficial de Kotlin sobre colecciones](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/-collection/).

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

Por lo general solo es necesario definir un constructor por defecto, pero en caso de que lo necesites te dejamos [este artículo que explica cómo escribir constructores secundarios](https://kotlinlang.org/docs/classes.html#secondary-constructors).


# Herencia y redefinición de métodos

A continuación vemos cómo definir Golondrina como subclase de Ave.

![image](/img/wiki/kotlin-inheritance.gif)

```kotlin
open class Ave() {
    ...
    open fun esFeliz() = energia < ENERGIA_MINIMA
}


class Golondrina : Ave() {
    override fun esFeliz() = true
}
```

Aquí vemos que

* Golondrina hereda de Ave, indicado mediante el símbolo `:`
* Golondrina debe llamar al constructor de Ave, que al no tener parámetros se indica por el momento con paréntesis vacíos: `class Golondrina : Ave()`
* Golondrina **redefine** el comportamiento de esFeliz, lo pisa, y esto requiere la palabra clave `override`, de lo contrario el IDE mostrará un mensaje de error
* Para que una clase pueda subclasificarse Kotlin obliga a utilizar la palabra clave `open`. Una segunda variante es que la clase sea abstracta en cuyo caso automáticamente es abierta.
* La misma operatoria debe seguir un método: debe marcarse con la palabra clave `open` (como en el caso `esFeliz`) para poder redefinirse en las subclases, a menos de que el método sea abstracto. Esto es un poco burocrático y extraño para el objetivo general que suele tener Kotlin, pero por el momento es así.

## Torcaza: This y super

Si queremos definir una clase Torcaza que redefina el comportamiento de volar pero que además delegue el comportamiento en la superclase, debemos utilizar la palabra clave `super` junto con el mensaje a enviar:

```kotlin
class Torcaza : Ave() {
    var vecesQueVolo = 0
    override fun volar() {
        super.volar()
        vecesQueVolo++
    } 
}
```

Como regla general solo deben utilizar `super` cuando no puedan utilizar `this`, como en este caso: de lo contrario entrarían en loop infinito si invocaran a `this.volar()`.

## Constructores delegados

Si la clase Ave se definiera de la siguiente manera:

```kotlin
open class Ave(var energia: Int = 0) {
```

eso no produciría ningún cambio en las definiciones de Golondrina y Torcaza ya que en cada invocación tomaría el valor por defecto de energía:

```kotlin
class Torcaza : Ave() { // considera energia = 0
```

Ahora bien, si la definición del constructor en Ave no tuviera valor por defecto:

```kotlin
open class Ave(var energia: Int) {
```

Entonces es necesario redefinir el constructor por defecto para Golondrina y Torcaza y pasarle ese valor al constructor de Ave. Esto se hace de la siguiente manera:

```kotlin
class Golondrina(energia: Int) : Ave(energia) {
```

Si bien esto puede convertirse en algo tedioso, veremos que el IDE nos simplifica bastante esta tarea, utilizando `Alt` + `Enter` para aceptar la sugerencia:

![Kotlin - Delegación de constructores](/img/wiki/kotlin-constructorInheritance.gif)

# Clases y métodos abstractos

Podemos definir a Ave como clase abstracta, esto producirá que no podamos instanciar objetos Ave. Una clase abstracta puede definir solo la interfaz de un método, lo que se conoce como método abstracto. Veamos el siguiente ejemplo:

![image](/img/wiki/kotlin-abstractClass.gif)

En el ejemplo:

* primero definimos Ave como abstracta
* eso provoca que el compilador tire un error cuando queremos instanciar un Ave en la clase Ornitologo
* lo corregimos instanciando una Golondrina
* luego, queremos definir un método abstracto: esFeliz. Para ello reemplazamos la definición por una cáscara que solo dice que esFeliz debe devolver un booleano. Dado que no hay código Kotlin nos fuerza a definir el tipo de retorno del método (y de sus parámetros) porque no puede inferirlo.
* todos los métodos abstractos deben estar implementados en las subclases: el compilador nos avisa que falta la definición de esFeliz() en Torcaza. Con un botón derecho "Implement members" pegamos la definición copiada.

y finalmente todo compila.

Te dejamos el código completo:

```kotlin
val ENERGIA_MINIMA = 10

abstract class Ave(var energia: Int) {
    open fun volar() { energia = energia - 10 }
    fun comer(cuanto: Int) { energia = energia + (cuanto * 2) }
    abstract fun esFeliz(): Boolean
    fun resetearEnergia() { energia = 0 }
}

class Golondrina(energia: Int) : Ave(energia) {
    override fun esFeliz() = true
}

class Torcaza(energia: Int) : Ave(energia) {
    var vecesQueVolo = 0
    override fun volar() {
        super.volar()
        vecesQueVolo++
    }

    override fun esFeliz() = energia < ENERGIA_MINIMA
}

class Ornitologo {
    fun trabajar() {
        val ave = Golondrina(energia = 100)
        ave.comer(2)
        ave.volar()
    }
}
```

# Interfaces

Las interfaces son un mecanismo que permite definir un **contrato**, provisto por una serie de métodos que pueden o no estar definidos. Por ejemplo, veamos la interfaz `Flying` que expresa el contrato para cualquier elemento que sepa volar:

```kotlin
interface Flying {
    fun isHappy(): Boolean
    fun fly()
}
```

Esto implica que cualquier definición que **implemente** la interfaz `Flying` debe poder responder a esos dos mensajes: isHappy() y fly(). Por ejemplo, la clase `Bird`, donde el símbolo `:` sirve tanto para marcar herencia como implementación:

```kotlin
interface Flying {
    fun fly()
    fun isHappy(): Boolean
}

// clase Bird implementa Flying
class Bird(var energy: Int = 100) : Flying {
    fun eat(howMuch: Int) { energy = energy + (howMuch * 2) }
    fun resetEnergy() { energy = 0 }
    override fun fly() { energy = energy - 10 }
    override fun isHappy() = energy > MIN_ENERGY
}
```

Cada método implementado debe anotarse con el prefijo `override` para indicar que está implementando los métodos que le pide su interfaz.

¿Por qué `Flying` no se define como clase abstracta? Podríamos, pero mientras que una clase solo tiene una superclase puede implementar varias interfaces a la vez. Supongamos que ahora definimos la interfaz `Living` para representar seres vivos:

```kotlin
interface Living {
    var energy: Int
    fun eat(howMuch: Int)
}
```

Living define un atributo sin ningún valor concreto, ya que **no puede definir un estado, a diferencia de la clase abstracta**. Ahora `Bird` puede implementar ambas interfaces, para lo cual tiene que indicar que va a redefinir el atributo `energy` y todos los métodos abstractos requeridos por las interfaces `Flying` y `Living`:

```kotlin
// clase Bird implementa las interfaces Flying y Living
class Bird(override var energy: Int = 100) : Flying, Living {
    override fun eat(howMuch: Int) { energy = energy + (howMuch * 2) }
    fun resetEnergy() { energy = 0 }
    override fun fly() { energy = energy - 10 }
    override fun isHappy() = energy > MIN_ENERGY
}
```

Por último, las interfaces permiten definir implementaciones para los métodos, como podemos ver en este ejemplo completo:

```kotlin
val MIN_ENERGY = 100

interface Flying {
    fun fly()
    fun isHappy(): Boolean
    fun canFly() = !isHappy()
}

interface Living {
    var energy: Int
    fun eat(howMuch: Int) { energy = energy + (howMuch * 2) }
    fun resetEnergy() { energy = 0 }
}

class Bird(override var energy: Int = 100) : Flying, Living {
    override fun fly() { energy = energy - 10 }
    override fun isHappy() = energy > MIN_ENERGY
}
```

Aquí vemos que cuando le preguntemos a un pájaro si puede volar, la definición la tomará de la implementación de `Flying`. Por otra parte cuando le pidamos a un pájaro que coma, lo hará en base a la definición de la interfaz `Living`. De todas maneras hay que estar seguro de que vamos a reutilizar en más de un lugar cada una de nuestras definiciones para no caer en el sobrediseño.


> **TIP**: A la hora de reutilizar, una interface nos permite tomar definiciones de múltiples lugares aunque no permite definir un estado mientras que una superclase abstracta nos permite definir una sola vez nuestros atributos aunque solo podemos tener una superclase.


# Bloques

Un bloque permite definir una porción de código, también llamada **expresión lambda**:

```kotlin
val cuadrado = { num: Double -> num.pow(2) }
cuadrado.invoke(5.0)  // 25
```

En este caso cuadrado es un bloque que recibe como parámetro un número con decimales y devuelve el cuadrado de dicho número. Si queremos definir el tipo de dato de cuadrado podemos:

```kotlin
val cuadrado: (Double) -> Double = { num: Double -> num.pow(2) }
cuadrado.invoke(5.0)  // 25
```

En general un bloque en Kotlin tiene la siguiente sintaxis:

```kotlin
{ parametro: Tipo, parametro2: Tipo2 -> expresión a resolver }
```

De esta manera podemos enviar bloques como parámetros, algo muy útil para trabajar entre otras cosas con las colecciones (`map`, `filter`, `fold`, etc.)


## Variable implícita it

Dentro de una expresión lambda, `it` es la variable implícita del primer parámetro, por lo tanto todas estas expresiones son equivalentes:

```kotlin
System.out.println(alumnos.filter { alumno: Alumno -> alumno.estudioso() })
System.out.println(alumnos.filter { it.estudioso() })
```

Para más información pueden consultar [la página oficial de lambdas de Kotlin](https://kotlinlang.org/docs/lambdas.html).

# Manejo de nulls

## 100 veces no debo

Los valores nulos son siempre un dolor de cabeza, Kotlin es uno de los primeros lenguajes orientados a objetos que nace con una estrategia para mitigarlos. En principio una referencia definida como String o Int **no acepta valores nulos**.

![Kotlin - String no acepta null](/img/wiki/kotlin-stringNotNull.png)

Ok, entonces podríamos pensar que una solución es sacar el `null` explícito, y si como dijo Iván Noble algunos errores son deliciosos, sin dudas uno es éste:

![Kotlin - no permite dejar sin inicializar](/img/wiki/kotlin-stringUninitialized1.png)

Debemos inicializar la referencia, ¡exacto! porque de otra manera lo que pasa es que arrastramos un String que puede ser `null` por todo nuestro código, solo por no tomar la decisión de que ese valor **nunca puede ser nulo**.

## Lateinit

Una variante para resolver el problema es definir el atributo como `lateinit`:

```kotlin
class Persona {
    lateinit var nombre: String
    fun tieneNombreLargo() = nombre.length > 10
}
```

El efecto que provoca es que Kotlin confía en que nosotros vamos a definir siempre un valor para el nombre de cada persona antes de utilizarlo. Por ejemplo:

```kotlin
fun main() {
    val pepe = Persona()
    pepe.nombre = "Pepe"
    System.out.println(pepe.tieneNombreLargo())  // false
}
```

Y no hay ningún inconveniente. ¿Qué pasa si en cambio probamos con esta variante?

```kotlin
fun main() {
    val pepe = Persona()
    System.out.println(pepe.tieneNombreLargo())
}
```

Kotlin se va a quejar de que nosotros le dijimos "quedate tranquilo que yo me ocupo del nombre" y resultó que el nombre quedó sin inicializar:

```bash
Exception in thread "main" kotlin.UninitializedPropertyAccessException: lateinit property nombre has not been initialized
 at Persona.getNombre (File.kt:2) 
 at Persona.tieneNombreLargo (File.kt:3) 
 at FileKt.main (File.kt:8)
```

Más adelante, cuando trabajemos con algunos frameworks como Spring, veremos que el modificador `lateinit` nos va a ser de mucha utilidad. Mientras tanto, cuando nosotros controlamos la inicialización de las referencias para cada objeto, la mejor estrategia es definir un valor no-nulo por defecto:

```kotlin
class Persona {
    var nombre: String = ""
    fun tieneNombreLargo() = nombre.length > 10
}
```

## Valores que aceptan null

Para aceptar valores `null` todos los tipos deben incorporar el sufijo `?`, por ejemplo `String?`, `Int?`, etc.

```kotlin
class Persona {
    var nombre: String? = null
    ...
```

El inconveniente es que para saber si una persona tiene nombre largo, tenemos que considerar ahora si tiene un nombre nulo:

![Kotlin - Non null safe operation](/img/wiki/kotlin-nonNullSafe.png)

### Operador !!

Una opción es utilizar el operador `!!` sobre nombre, que implica nuevamente confiar en que el nombre no va a ser nulo:

```kotlin
fun tieneNombreLargo() = nombre!!.length > 10
```

Esto implica que anulamos la validación y nos puede pasar lo mismo que en otros lenguajes como Java: al enviar un mensaje a una referencia nula el programa explota en tiempo de ejecución.

```bash
Exception in thread "main" java.lang.NullPointerException
 at Persona.tieneNombreLargo (File.kt:3) 
 at FileKt.main (File.kt:8) 
 at FileKt.main (File.kt:-1) 
```

### Elvis operator

Parece un emoticón, pero `?:` es un shortcut para utilizar un valor por defecto cuando una expresión pueda ser nula:

```kotlin
fun tieneNombreLargo() = (nombre ?: "").length > 10
```

En este caso, si la referencia nombre no está inicializada, se toma en cuenta la segunda expresión (el string vacío).

## Null safe operator

También podemos resolver envíos de mensajes a referencias que potencialmente podrían ser nulas:

```kotlin
class Alumno(var nombre: String = "") {
    fun estudioso() = ...
    fun felicitar() { ... }
}

fun main() {
    val alumnos = listOf(Alumno(nombre = "Valar"), Alumno(nombre = "Arya"))
    val estudioso = alumnos.find { it.estudioso() }
    System.out.println(estudioso?.nombre) // null
    estudioso?.felicitar()
}
```

Si estamos buscando información del primer alumne estudiose (o de algune) enviando el mensaje `find` a la colección puede pasar que la búsqueda no encuentre ningún elemento. En ese caso el operador `?.` es equivalente a escribir:

```kotlin
val estudioso = alumnos.find { it.estudioso() }
System.out.println(if (estudioso === null) null else estudioso.nombre) // null
if (estudioso !== null) {
    estudioso.felicitar()
}
```

pero como vemos es bastante menos tedioso de escribir. De todas maneras cuando sea posible es una buena práctica evitar la manipulación de tipos de datos con valores nulos, porque no siempre se puede resolver mágicamente con un `?` cualquier operación:

![Kotlin - null safe no válido](/img/wiki/kotlin-nullSafeOperatorNotAllowed.png)

Entonces el consejo que te dejamos es **solo dejar valores que acepten nulls cuando el negocio realmente lo necesite**. Por ejemplo: si un perro puede tener dueño o no, entonces el atributo puede ser nullable.

## Comparar referencias

Tenemos dos formas de comparar referencias en Kotlin:

* **Igualdad referencial**: definido por el operador `===`. `ref1 === ref2` si ambas referencias apuntan al mismo objeto. Esto lo determina la VM y no se puede cambiar.
* **Igualdad estructural**: definido por el operador `==`. `ref1 == ref2` en base a la definición del método `equals()` en la clase a la que pertenece ref1.

**Tener especial atención a los strings**, ya que dos strings con el mismo contenido pueden ser iguales pero no idénticos, dependiendo de las estrategias de optimización de la VM. Vemos un ejemplo ilustrativo:

```kotlin
fun main() {
    val nombre = "Ernesto"
    val nombre2 = "Ernesto ".trim()
    System.out.println(nombre == nombre2)   // true, tienen el mismo contenido
    System.out.println(nombre === nombre2)  // false, no son el mismo objeto
}
```

> **Tip:** Siempre es conveniente utilizar ==, que además se puede redefinir en nuestras clases / objetos. 


# Features avanzados

## Extension methods

Una de las herramientas más poderosas consiste en definir **extension methods**. Supongamos que un negocio tiene un horario de apertura y de cierre y queremos saber, dada una hora, si está abierto.

```kotlin
class Negocio {
    var horarioApertura: Int = 9
    var horarioCierre: Int = 18

    fun estaAbierto(horaActual: Int) =
        horaActual.between(horarioApertura, horarioCierre)
}
```

Por supuesto, no compila. No existe el método between asociado a los enteros. Pero podemos definir un **extension method** en cualquier archivo:

```kotlin
fun Int.between(from: Int, to: Int) = this in from..to
```

Si definiste la extensión en otro paquete, lo importás como cualquier otra definición:

```kotlin
package otroPackage

import between

class Negocio {
    ...
```

En resumen, un extension method permite que nosotros agreguemos comportamiento por afuera de la definición de una clase como si estuviéramos trabajando en ella, algo muy importante cuando la clase no podemos modificarla (como en el caso de Int, String), o bien cuando se está regenerando todo el tiempo (cuando tenemos un framework que genera código para nosotros), sin contar que además estamos respetando la idea de **mensaje** (y por consiguiente, la posibilidad de seguir trabajando con polimorfismo).


# Data classes

Kotlin provee el concepto de **Data class** para definir clases que sirven para modelar valores (_value objects_):

```kotlin
data class Point(val x: Int, val y: Int) {
    // ... definiciones adicionales ...
}

fun main() {
    val punto = Point(2, 4)
    System.out.println(punto.x)               // 2
    System.out.println(punto)                 // Point(x=2, y=4)
    System.out.println(punto == Point(2, 4))  // true
}
```

Aquí vemos que el **data class Point**

- define un constructor con dos parámetros que a su vez definen las variables x e y
- los getters para x e y existen automáticamente
- como x e y son `val` esto hace que nuestro objeto Point sea **inmutable**, si sumamos dos puntos obtenemos un nuevo punto (como pasa al concatenar los strings "hola΅ y "mundo" donde se obtiene un nuevo string "holamundo" o al sumar 2 + 3 el resultado es un nuevo número 5)
- si definimos x (o y) como `var`, Kotlin le agrega los setters correspondientes
- el método `toString` de un data class que crea Kotlin es muy conveniente, permite mostrar tanto la clase como su estado interno (a comparación del `toString` por defecto que tiene Object que muestra solo el nombre de la clase y un número interno en formato hexadecimal)
- y por último también redefine el método `equals` de manera de utilizar igualdad estructural: dos puntos son iguales si tienen la misma información, porque cuando modelamos _value objects_ es frecuente crear objetos para representar ciertos datos y después se descartan

> **Tip:** qué objetos son candidatos a modelarse con data class: un Mail, un domicilio, en general cuando estamos agrupando información que está junta pero que no es específica de un dominio, la identidad no es importante como pasa cuando definimos objetos cliente, producto, etc.

# Operadores para procesar múltiples envíos de mensajes

Otro syntactic sugar muy interesante de Kotlin es la posibilidad de enviar múltiples mensajes al mismo objeto, mediante varios operadores:

- apply
- let
- with
- run
- also

```kotlin
val ventaNacional = Venta().apply {
    cantidadKilos = 12
    fechaVenta = LocalDate.now()
    parcela = parcela50
    comprador = CompradorNacional()
}
```

De esta manera, todos los mensajes se apuntan al objeto que resulta de evaluar la expresión `Venta()`, y simplifica el envío de mensajes: 

```kotlin
ventaNacional.cantidadKilos = 12
ventaNacional.fechaVenta = LocalDate.now()
ventaNacional....
```

## Otras variantes

Las _scope functions_ `let`, `also`, `run` y `with` son similares pero tienen ligeras variaciones para lo que sea más conveniente en cada caso:

### Let

El valor que le pasamos como parámetro se referencia como `it` y lo que devuelve es el resultado de toda la operación:

```kotlin
Venta().let { it.cantidadKilos * it.parcela.tamanio } // devuelve un número
```

### With

El valor que le pasamos como parámetro se referencia como `this` y lo que devuelve es el resultado de toda la operación. También es útil para trabajar el ejemplo original de la creación de una venta diciendo "a este objeto enviale estos mensajes":

```kotlin
val ventaNacional = Venta()
with(ventaNacional) {
    cantidadKilos = 12
    fechaVenta = LocalDate.now()
    parcela = parcela50
    comprador = CompradorNacional()
}
```

Para más información (como las scope functions `run` y `also`) pueden ver [este artículo](https://kotlinlang.org/docs/scope-functions.html#functions)

# Links relacionados

* [Colecciones](https://docs.google.com/document/d/1lzOStySb8i94oVvZUIxkgymf2tuCDuXzqSTnClPqKSM/edit?usp=sharing)
* [Intro a manejo de errores con excepciones](https://docs.google.com/document/d/1G0a9j-OA0rIEA5cdvEhIMbztJVo86ssvZKBK8HL9akg/edit?usp=sharing)
* [Ejercicio del Monedero](https://docs.google.com/document/d/1vVW91adl0p-NxGNpe8fqmC_5YmBkrxaLDFKyZ0xZb9Y/edit?usp=sharing) para ver cómo interactúan la UI y el dominio a partir de errores del dominio y del sistema
* [Testing](https://docs.google.com/document/d/11mVR-4wEZhlQMDEqrfQeYLypEsrSqXv98dr78SA0Oq4/edit?usp=sharing)

# Links útiles

* [Volver al menú principal del entorno Kotlin](kotlin-principal.html)
