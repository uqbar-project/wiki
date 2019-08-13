---
layout: article
title: Xtend - Guía rápida
categories: [xtend, lenguaje, guia]
featured: true
---

![image](../../img/wiki/Xtend-logo.png)

# Guía rápida de Xtend

La siguiente es una guía de _syntactic sugars_ de Xtend, algunos de los cuales trabajan conceptos más profundos que veremos a lo largo de la materia.
___

## Definición de una clase

No tenemos objetos en Xtend, sólo clases. Aquí dejamos un ejemplo

```xtend
class Ave {
    static int ENERGIA_MINIMA = 10
    @Accessors int energia = 0
    def volar() { energia = energia - 10 }
    def comer(int cuanto) { energia = energia + (cuanto * 2) }
    def esFeliz() { energia > ENERGIA_MINIMA }
    def resetearEnergia() { energia = 0 }
}
```

* podemos escribir múltiples clases en un archivo Xtend
* la definición de la clase se encierra entre llaves
* la variable ENERGIA_MINIMA es estática, esto significa que **todas las instancias comparten la misma variable** (lo que también se llama variable de clase), suele usarse como constantes o valores que difícilmente cambien.
* la variable energia **es una variable de instancia** porque cada objeto Ave tiene su propio valor.
* las variables de instancia y de clase deben anotarse con el tipo, en este caso ambos son int
* la variable `energia` tiene la anotación `@Accessors`, que permite definir getters y setters automáticos
* los métodos volar, comer y esFeliz necesitan la palabra `def` para indicar que son métodos. Algunos producen efecto (volar y comer) y otros simplemente devuelven un valor (esFeliz)
* no es necesaria la palabra `return` al final de cada método, pero en algunos casos veremos que es necesaria
* no siempre es necesario hacer anotaciones de tipo sobre los métodos, como veremos más adelante

## Referencias variables y valores

En Xtend, al igual que muchos otros lenguajes, se diferencian las referencias como

* **Variables**: son referencias que pueden inicializarse apuntando a un objeto, y luego reasignarse a otro. Justamente "varían":

```xtend
var String unString = "Pepito"
unString = "Otro String"
```

* **Constantes**: son referencias que nacen apuntando a un valor y no pueden ser modificadas para apuntar a otro objeto. Serían como "constantes".

```xtend
val String constante = "Constante"
constante = "Otro"  // <----- NO COMPILA !
```

¡Ojo! no confundir el hecho de que no se pueda modificar la "referencia" de la mutabilidad/inmutabilidad del objeto al que apunta. Puedo tener un "val" apuntando a una colección, que es mutable.

```xtend
val List miLista = unaLista
miLista = otraLista  // <----- NO COMPILA: no puedo modificar la referencia
miLista.add(23)      // <---- SI COMPILA: puedo mandarle mensajes al objeto lista y agregarle elementos
```

### Cuándo debería usar val y cuándo var

* las variables de instancia siempre son variables, de lo contrario no tiene sentido definirlas como tal
* los valores constantes se suelen definir como variables de clase o static
* respecto a las variables locales, en la cursada las usaremos pocas veces: primero intentaremos definirlas como `val` y de no ser posible, como `var`. El motivo principal es acotar el efecto en nuestros programas, **mientras menor sea el efecto, más fácil es controlar nuestro software, y más fácil será testearlo**.

## Tipos de datos

### Strings

Un string se encierra entre dobles comillas, o bien podemos aprovechar para escribir un texto largo con enters con triples comillas simples, e insertar en el medio código Xtend mediante

```xtend
class Cliente {
    var nombre = "Juan" // definición con comillas dobles

    def presentacion() {
        '''
        Bienvenido, «nombre.trim()» a nuestra aplicación.
        En breve nos contactaremos con ud.
        '''
    }
}
```

> **Tip**: para que te aparezcan los símbolos `«»` que son difíciles de encontrar en el teclado, simplemente utilizá las teclas `Ctrl` + `Espacio` dentro de la definición del string y aparecerán solas

### Números

Existen muchos tipos de datos diferentes para números:

* **int**: es un número entero que admite negativos pero sin decimales
* **double**, **float**: son números reales que admiten decimales pero con errores en las operaciones, es por ello que no debemos usarlo para operaciones sensibles (como transacciones bancarias o que requieran cálculos exactos)
* **BigDecimal**: es el tipo de dato que conviene utilizar ya que no produce errores de redondeo (permite trabajar con una cantidad exacta de decimales y truncarlos o redondearlos en caso de ser necesario)
* También existen las variantes "objetosas" de int, double y float que son Integer, Double y Float. La principal ventaja es que son objetos, y podemos enviarle mensajes (concretamente más mensajes) que a las versiones en minúscula, que son _tipos primitivos_. Es posible "envolver" en una variable Integer cualquier int, la conversión se da automáticamente por el compilador de Java y se llama "autoboxing":

```xtend
var i = 0                         // int
var pi = 3.14d                    // double
var saldo = new BigDecimal(1500)  // BigDecimal
var Integer otroI = i             // otroI es un entero
otroI.bitwiseNot                  // puedo enviar un mensajes
```

### Colecciones

Existen literales para definir listas, conjuntos y mapas (dictionaries):

```xtend
// Lista inmutable:
val myList = #['Hello', 'World']
// Set inmutable
val mySet = #{'Hello', 'World'}
// Mapa/Diccionario inmutable
val myMap = #{'a' -> 1 , 'b' ->2}
```

Recordemos que

* **listas**: respetan el orden en el que se agregan (como una fila) y admiten duplicados.
* **conjuntos**: no tienen orden y tampoco admiten duplicados. Dos objetos son iguales en base a la definición de equals() y hashCode().
* **mapas**: también llamados dictionaries, son un conjunto de pares clave/valor. Se acceden por clave.

### Range

Es posible generar un rango de números, por ejemplo para iterar una cantidad de veces:

```xtend
#[1 .. 10].forEach [ ... ]      // [1..10] genera la lista de 1 a 10
#[1 ..< 10].forEach [ ... ]     // [1..<10] genera la lista de 1 a 9
#[1 >.. 10].forEach [ ... ]     // [1>..10] genera la lista de 2 a 10
```

`..<` es útil cuando necesitás iterar una lista de Java, que comienza en 0 y termina en (longitud - 1):

```xtend
#[0 ..< lista.size].forEach [ i | println(i) ]
```

### Literales para lista, conjunto, etc.

Xtend trae shortcuts para definir diferentes tipos de colecciones:

```xtend
List<Factura> facturas = newArrayList
Set<Domicilio> domicilios = newHashSet
List<String> nombres = newArrayList("nahuel", "rodrigo", "marina")
```

Podés utilizar `newLinkedList`, `emptyList`, `emptySet`, `emptyMap`, `newInmutableMap`, `newImmutableSet`, `newImmutableList`, `newLinkedHashSet`, `newTreeSet`, `newHashMap`, `newLinkedHashMap`, `newTreeMap`. La ventaja que tienen es que permiten pasarle parámetros variables (tantos como elementos necesites) y trae implementaciones por defecto para algunas colecciones que necesitan comparators (tenés que estudiar más a fondo [**Colecciones en Xtend**](https://docs.google.com/document/d/1lzOStySb8i94oVvZUIxkgymf2tuCDuXzqSTnClPqKSM/edit?usp=sharing))

### Inferencia de tipos

Xtend cuenta con inferencia de tipos, lo que permite

* que existan chequeo de tipos
* pero que la mayoría de las veces no sea necesario definir los tipos de las expresiones

Vemos un ejemplo en vivo, mostrando cómo cambia la solapa "Outline" cuando modificamos el código:

![image](../images/xtendTypeInference.gif)

Aquí vemos que incluso Xtend detecta expresiones que no tienen sentido, como cuando hicimos:

```xtend
def esFeliz() {
    energia > ENERGIA_MINIMA
    "si"
}
```

El hecho de generar una expresión `energia > ENERGIA_MINIMA` no causa efecto en el objeto y tampoco se devuelve (porque se pisa por la expresión "si" que es devuelta como retorno del método).

Volviendo a la inferencia de tipos, es fundamental poder contar con un lenguaje que tenga chequeo de tipos para detectar errores en forma temprana pero **que no me obligue a definir los tipos todo el tiempo**. La definición de tipos es obligatoria para las variables de instancia y de clase de los objetos, y en algunos casos cuando la definición de métodos polimórficos puede resultar ambigua para Xtend. En cualquiera de esos casos vas a ver un mensaje de error o de advertencia para que definas el tipo que mejor se ajuste.

### Casteos

Si bien toda expresión tiene un tipo y Xtend suele inferirlo bastante bien, a veces es necesario hacer _downcasting_ o forzar que una expresión pase por un tipo de datos:

```xtend
(42 as Integer)
(cliente as Cliente)
```

En general la expresión es, entre paréntesis: (`valor/variable` as `tipo`)

## Definición de propiedades

La anotación @Accessors puede hacerse sobre una variable, como hemos visto antes:

```xtend
class Ave {
    @Accessors int energia = 0
```

En este caso se crean getters y setters para energia, **transformándolo en una propiedad**.

```xtend
class Ave {
    @Accessors(PUBLIC_GETTER) int energia = 0
```

En este caso se crea un getter público para la variable energia. Otras variantes son: crear sólo un setter público o crear getters o setters con diferentes visibilidad.

Por último, podemos anotar la clase con @Accessors

```xtend
@Accessors
class Ave {
    int energia = 0
    int vecesQueVolo = 0
```

en este caso, se crean getters y setters para todas las variables de dicha clase.

## Shortcut para acceder a propiedades

Cuando usamos un objeto que tiene propiedades (par getter y setter), podemos cambiar un poco la sintaxis para que se vea más simple. En el ejemplo anterior del Ave:

```xtend
val pepita = new Ave()
pepita.energia = 100    // <-- equivale a pepita.setEnergia(100)
pepita.energia          // <-- equivale a pepita.getEnergia()
```

¡Ojo! si bien parece que estamos accediendo diréctamente a la variable de instancia, no es así. Xtend simplemente traduce esa sintaxis a la anterior. Es decir que en ambos casos estamos igualmente llamando al getter y al setter. Pueden probar eliminando la anotación @Accessors y recibiremos un mensaje "The field energia is not visible".

## Paréntesis en el envío de mensajes

No es necesario utilizar paréntesis, ni en la creación de objetos, ni en el envío de mensajes sin parámetros:

```xtend
val golondrina = new Ave
golondrina.resetearEnergia
```

## Herencia y redefinición de métodos

A continuación vemos cómo definir dos subclases de Ave: Golondrina y Torcaza.

![image](../images/xtendInheritanceShort.gif)

```xtend
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

## Clases y métodos abstractos

Podemos definir a Ave como clase abstracta, esto producirá que no podamos instanciar objetos Ave. Una clase abstracta puede definir solo la interfaz de un método, lo que se conoce como método abstracto. Veamos el siguiente ejemplo:

![image](../images/xtendAbstractClassesAndMethods.gif)

En el ejemplo:

* primero definimos Ave como abstracta
* eso provoca que el compilador Xtend tire un error cuando queremos instanciar un Ave en la clase Ornitologo
* lo corregimos instanciando una Golondrina
* luego, queremos definir un método abstracto: esFeliz. Para ello reemplazamos la definición por una cáscara que solo dice que esFeliz debe devolver un booleano. Dado que no hay código Xtend nos fuerza a definir el tipo de retorno del método (y de sus parámetros) porque no puede inferirlo.
* todos los métodos abstractos deben estar implementados en las subclases: el compilador nos avisa que falta la definición de esFeliz() en Torcaza. Con un botón derecho "Add unimplemented methods" pegamos la definición copiada.
* como nos falta la constante que estaba en Ave, la bajamos mediante `Alt` + `Flecha abajo`

y finalmente todo compila.

## Constructores

Un constructor se define con la palabra reservada `new` (equivalente al `constructor` de Wollok):

```xtend
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

## Bloques

Un bloque permite definir una porción de código, también llamada expresión lambda:

```xtend
val cuadrado = [ int num | num ** 2 ]
cuadrado.apply(5)
```

De esta manera podemos enviar bloques como parámetros, algo muy útil para trabajar entre otras cosas con las colecciones (`map`, `filter`, `fold`, etc.)

La sintaxis general es

```xtend
[ | ... ]                // bloque sin parámetros
[ elem | ... ]           // bloque con un parámetro
[ int a, int b | a + b ] // bloque con dos parámetros
```

### Variable implícita it

De la misma manera que cuando estamos dentro de una clase, podemos acceder a una variable de instancia con `this`

```xtend
this.energia
```

o sin él:

```xtend
energia
```

también podemos usar una variable implícita `it` dentro de un método.

```xtend
val it = new Ave()
volar       // equivale a it.volar()
comer(2)    // equivale a it.comer(2)
```

Dentro de una expresión lambda, `it` es la variable implícita del primer parámetro, por lo tanto todas estas expresiones son equivalentes:

```xtend
alumnos.filter [ alumno | alumno.estudioso() ]
alumnos.filter [ it | it.estudioso() ]
alumnos.filter [ it.estudioso() ]
alumnos.filter [ it.estudioso ]
alumnos.filter [ estudioso ]
```

![image](../images/xtendItImplicitVariable.png)

## Manejo de nulls

Los valores nulos son siempre un dolor de cabeza, Xtend tiene algunos trucos para facilitar un poco más el trabajo con ellos.

### Elvis operator

Parece un emoticón, pero `?:` es un shortcut para utilizar un valor por defecto cuando una expresión pueda ser nula:

```xtend
val nombre = person.firstName ?: 'You'
```

Si la expresión que está a la izquierda se evalúa como null, `nombre` se asigna a la segunda expresión.

### Null safe operator

También podemos resolver envíos de mensajes a referencias que potencialmente podrían ser nulas:

```xtend
val mejorAlumno = alumnos.find [ ... ]
...
mejorAlumno?.felicitar()
```

En este caso, el operador `?.` es equivalente a preguntar `if (mejorAlumno) mejorAlumno.felicitar()`

### Comparar referencias

Después de varios cambios, Xtend dejó las cosas como la mayoría de los lenguajes. Tenemos dos formas de comparar referencias:

```xtend
ref1 == ref2     // compara por igualdad, esto significa que son iguales si son las referencias
                 // apuntan al mismo objeto o bien, en base a la definición del método equals
                 // en la clase ref1 (sabiendo que ref1 no es nulo)
                 // en la clase asociada a ref1
ref1 === ref2    // compara por identidad, esto significa que son iguales si las referencias
                 // apuntan al mismo objeto en memoria, determinado por la VM y no se puede cambiar
```

**Tener especial atención a los strings**, ya que dos strings con el mismo contenido pueden ser iguales pero no idénticos, dependiendo de las estrategias de optimización de la VM. Siempre es conveniente utilizar ==, que además se puede modificar.

## Métodos avanzados

### Obligatoriedad del return en métodos

Por lo general, los métodos devuelven la última expresión que contienen. Pero a veces es necesario cortar el flujo de envío de mensajes, como por ejemplo aquí:

```xtend
def gradoDeFelicidad() {
    if (!esFeliz) {
       return 0
    }
    ... cálculo complejo ...
}
```

Para determinar el grado de felicidad de alguien, tenemos como precondición que sea feliz. Y para simplificar la definición, escribimos el `if` y forzamos el return (dado que escribir únicamente `0` no tendrá efecto, porque Xtend seguirá evaluando el resto de las expresiones hasta terminar la última y nosotros queremos justamente cortar el flujo).

En el caso de un método que solo busque producir un efecto (`void`), es necesario utilizar `return;` con punto y coma...

```xtend
def metodoConEfecto() {
    ... cambios ...
    if (!situacion) {
       return;
    }
    ... otros cambios ...
}
```

Igualmente, siempre es preferible tratar de extraer métodos más pequeños para simplificar la lógica.

### Extension methods

Una de las herramientas más poderosas consiste en definir **extension methods**. Supongamos que un negocio tiene un horario de apertura y de cierre y queremos saber, dada una hora, si está abierto.

```xtend
class Negocio {
    int horarioApertura
    int horarioCierre

    def estaAbierto(int horaActual) {
        horaActual.between(horarioApertura, horarioCierre)
    }
}
```

Por supuesto, no compila. No existe el método between asociado a los enteros. Pero en otro archivo vamos a definir un método estático (no asociado a un objeto):

```xtend
class NumberUtils {
    static def between(int num, int from, int to) {
        num >= from && num <= to
    }
}
```

Y ahora, desde el archivo Negocio.xtend, vamos a importar el método como `static extension`:

![image](../images/xtendExtensionMethod.gif)

El código a agregar es

```xtend
// después de la definición de package
import static extension ar.edu.unsam.prueba.NumberUtils.*
```

Esto produce que automáticamente, el compilador Xtend marque en naranja el método between y nuestra clase Negocio compile perfectamente. En resumen, un extension method permite que nosotros agreguemos comportamiento por afuera de la definición de una clase como si estuviéramos trabajando en ella, algo muy importante cuando la clase no podemos modificarla (como en el caso de Integer , String), o bien cuando se está regenerando todo el tiempo (cuando tenemos un framework que genera código para nosotros), sin contar que además estamos respetando la idea de **mensaje** (y por consiguiente, la posibilidad de seguir trabajando con polimorfismo).

Los métodos `map`, `filter`, `fold`, `length`, `any`, etc. son todos extension methods de Collections.

### Dispatch methods

Xtend permite trabajar con **multimethods**, más adelante tendremos [este ejercicio para contarlo con más profundidad](https://docs.google.com/document/d/1XWq9azqchoJZ7h8-hLcpA1Zj5T1UtvFtDKbpzxoQ-dw/edit?usp=sharing)

## @Data

Para definir un objeto inmutable, debemos:

* generar una clase
* con atributos privados
* y un constructor donde pueda asignar ese estado

Xtend provee la anotación @Data para lograr eso:

```xtend
@Data
class Point {
    int x
    int y
}
```

Esto equivale a definir el constructor con dos parámetros (x, y) y los getters para los atributos x e y, sin setters (dado que queremos únicamente representar un valor). Por lo tanto, esta definición compila perfectamente:

```xtend
class TestPoint {
    def test() {
        new Point(2, 4).x
    }
}
```

Si por el contrario intentamos asignar el valor de `x`:

```xtend
new Point(2, 4).x = 2
```

nos dirá `The field x is not visible`.

## With operator

Otro syntactic sugar muy interesante de Xtend es la posibilidad de enviar múltiples mensajes al mismo objeto, mediante el operador with `=>`, algo muy útil cuando estamos instanciando objetos:

```xtend
val ventaNacional = new Venta => [
    cantidadKilos = 12
    fechaVenta = new Date
    parcela = parcela50
    comprador = new CompradorNacional
]
```

De esta manera, todos los mensajes se apuntan al objeto que resulta de evaluar la expresión `new Venta`, y simplifica el envío de mensajes: 

```xtend
ventaNacional.cantidadKilos = 12
ventaNacional.fechaVenta = new Date
ventaNacional....
```
