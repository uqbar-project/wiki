---
layout: article
title: Testeo unitario avanzado
---


Este artículo presenta algunas guías para desarrollar los casos de prueba, considerando que ya tienen una base de testeo unitario automatizado. Si estás buscando estudiar el tema en profundidad, te recomendamos el siguiente [apunte de Testing](https://docs.google.com/document/d/1zj-H_qPbUDvWsWhV2YLaPsnrI0MIkCuoeK_ilzSnxVI/edit?usp=sharing).

<br/>

Por otra parte, aquí explicamos la mecánica utilizando Kotest como framework de testeo, si estás buscando una variante que siga los lineamientos de JUnit, podés ver [esta página](testeo-unitario-avanzado.html).

# Ejemplo

Un sistema de seguros de automotor define en qué casos se puede pagar un siniestro:

- para los clientes normales, si no son morosos (la deuda debe ser 0)
- para las flotas de autos, se soporta una deuda de hasta $ 10.000 si el cliente tiene más de 5 vehículos ó hasta $ 5.000 en caso contrario

# Definiendo los escenarios

En base al ejemplo anterior, podemos considerar los siguientes escenarios:

- un cliente normal moroso: si debe $ 1 ó $ 50.000 no nos importa, porque está en la misma [clase de equivalencia](http://en.wikipedia.org/wiki/Equivalence_partitioning)
- una flota con menos de 5 autos (ó 5 autos) => serían "pocos" autos
- una flota con más de 5 autos => serían "muchos" autos

Elegimos cuántos autos en base al **valor límite**: como a partir de los seis autos se considera mucho y menos de 6 son "pocos" autos, 6 es el valor de una flota con muchos autos, 5 es el valor de una flota con pocos autos.

# Estructura de los tests

La estructura que tienen los tests en base a los escenarios propuestos podría ser:

```
dado un cliente normal
  ├── que es moroso: no puede cobrar un siniestro
  └── que no es moroso: puede cobrar un siniestro
dado un cliente de flota con muchos autos (6 autos)
  ├── si el cliente debe más de $ 10.000 no puede cobrar un siniestro
  └── si el cliente debe $ 10.000 o menos, puede cobrar un siniestro
dado un cliente de flota con pocos autos (5 autos)
  ├── si el cliente debe más de $ 5.000 no puede cobrar un siniestro
  └── si el cliente debe $ 5.000 o menos puede cobrar un siniestro
```

# Definiendo las especificaciones de los tests

Necesitamos

- un cliente normal
- una flota de 6 autos
- otra flota de 5 autos

a los que podemos configurar diferentes grados de deuda. Podemos seguir algunas recomendaciones adicionales:

## Agrupar los escenarios en diferentes archivos

Por el momento, no tenemos demasiados requerimientos. Entonces vamos a trabajar los tres escenarios desde el mismo archivo, al que llamaremos `CobroSiniestroSpec.kt` para explicitar el caso de uso que estamos testeando.

<br/>

A la hora de diseñar nuestros tests, hay dos ideas que están en tensión

- reutilizar nuestros escenarios, es decir, los objetos que estamos testeando
- que en cada test quede claro qué objetos participan de esa prueba (lo que se llama SUT, System Under Test)

<!-- -->
<br>

Por ejemplo, podríamos tener una flota con 6 autos y hacer tests para diferentes casos de uso: el cobro de un siniestro, el valor mensual de la cuota, el horario de atención, etc. El tema es que los tendremos en distintos archivos de test. La reutilización nos lleva a poner las cosas en un solo lugar, por ejemplo definiendo variables de instancia en una superclase común (o cualquier mecanismo que aumenta el alcance de la variable, volviéndola más global). Todo eso dificulta el entendimiento posterior del test, porque el código que se ejecuta previo a él está en varios lugares que además no son fáciles de rastrear. Más abajo veremos qué técnicas podemos utilizar para mantener nuestros tests simples.

<!-- -->
<br/>
Cada uno de los escenarios se implementa con un `describe` diferente, entonces tendremos 3 describes:

- uno para clientes normales, 
- otro para una flota con pocos autos 
- y otro para una flota con muchos autos

Es importante que no haya demasiados detalles de implementación en la descripción de los describes: "dada una flota con 5 autos" o "data una flota con 6 autos" provoca que cualquier cambio del negocio respecto a lo que son "muchos" o "pocos" autos necesite modificar esa descripción: es una duplicidad difícil de detectar.

### Intention revealing - parte 1

Queremos expresar lo más claramente posible **qué clase de equivalencia está testeando el describe**. Por eso comenzamos escribiendo:

```java
describe("Tests Cobro Siniestro") {
		describe("Dado un cliente normal") {
			...

		describe("Dada una flota con muchos autos") {
			...
```

Los describes agrupan los tests e incluso se pueden anidar, aunque por simplicidad solo vamos a utilizar un describe raíz para explicitar qué caso de uso estamos testeando. Una vez más recordamos: "muchos autos" es mejor que decir "6 autos". En otras palabras, explicitar el caso de prueba y no el dato de prueba: 6 autos es un dato concreto, pero lo que representa es el caso de prueba de una flota con muchos autos.

## Expresividad en los tests

### Un primer approach

Para crear nuestro fixture de una flota con muchos autos, los enunciados suelen traer ejemplos como: "Lidia Pereyra tiene una flota con 6 autos". Es tentador escribir un test como el siguiente:

```kotlin
describe("Lidia Pereyra") {
	val pereyra = Flota().apply {
		cantidadAutos = 6
	}
	it("no puede cobrar siniestro") {
		pereyra.generarDeuda(10001)
		pereyra.puedeCobrarSiniestro() shouldBe false
	}
	...
}
```

Pero ¿qué pasa si hay un error en el código de negocio? Supongamos esta implementación, donde la clase Cliente tiene la definición de la deuda como un entero:

```kotlin
class Flota : Cliente() {
    var autos: Int = 0

    override fun puedeCobrarSiniestro() =
        this.deuda <= maximoPermitido()

    fun maximoPermitido() =
        if (autos <= 5) 5000 else 20000 // debería ser 10000 en lugar de 20000

}
```

Cuando ejecutamos el test tenemos muy poca información relevante:

![Kotest - nombre de variable no representativa](/img/wiki/kotest-variable-no-representativa.png)

- la variable `pereyra` no está revelando que es un cliente de flota con muchos autos
- y tampoco está claro por qué no puede cobrar el siniestro el cliente. 

Al fallar la condición tenemos que bucear en el código y extraer este dato para determinar si el error está en el test o en el código de negocio. 

### Una segunda oportunidad

Vamos a mejorar la semántica del test, renombrando la variable `pereyra` por un nombre más representativo de la clase de equivalencia que estamos modelando y cambiando la descripción para el test:

```java
describe("Dada una flota con muchos autos") {
	val flotaConMuchosAutos = Flota()
	flotaConMuchosAutos.autos = 6
	it("si tiene mucha deuda no puede cobrar siniestro") {
		flotaConMuchosAutos.generarDeuda(10001)
		flotaConMuchosAutos.puedeCobrarSiniestro() shouldBe false
	}
```

Ahora al fallar el test sabemos más cosas:

![mas expresividad en los tests](/img/wiki/kotest-testeo-expresivo.png)

- el test con su stack trace, pero también
- qué es lo que estamos testeando, tratando de no entrar en detalles para no duplicar lo que dice el código

<!-- -->
<br/>

## AAA Pattern

Los tests suelen estructurarse según el patrón AAA: Arrange, Act y Assert.

```kotlin
describe("Dada una flota con muchos autos") {
	// Arrange
	val flotaConMuchosAutos = crearFlota(6)
	it("si tiene mucha deuda no puede cobrar siniestro") {
		// Act
		flotaConMuchosAutos.generarDeuda(10001)
		// Assert
		flotaConMuchosAutos.puedeCobrarSiniestro() shouldBe false
	}
	it("si no tiene poca deuda puede cobrar siniestro") {
		// Act
		flotaConMuchosAutos.generarDeuda(10000)
		// Assert
		flotaConMuchosAutos.puedeCobrarSiniestro() shouldBe true
	}
}
```

### Arrange

En el **A**rrange: donde instanciamos los objetos a testear, con sus colaboradores: en el ejemplo son la flota y sus autos. 

Instanciar un objeto adecuado para el test puede involucrar varios pasos, en ese caso es conveniente definir métodos _helpers_ que además puedan reutilizarse en diferentes contextos:

```kotlin
fun crearFlota(cantidadAutos: Int) =
	Flota().apply {
		autos = cantidadAutos
	}

...		

describe("Dada una flota con muchos autos") {
	// Arrange
	val flotaConMuchosAutos = crearFlota(6)
```

En el ejemplo tenemos un método _helper_ del test que permite crear un objeto Flota pasándole la cantidad de autos a crear. De esa manera la configuración de una flota ocurre en una sola línea y se puede incluir dentro del test mismo. El número 6 representa el **valor límite** para la flota, podríamos setearlo en base a una constante asociado a la clase Flota:

```kotlin
// clase Flota
val LIMITE_MUCHOS_AUTOS = 5

// el test
describe("Dada una flota con muchos autos") {
	// Arrange
	val flotaConMuchosAutos = crearFlota(LIMITE_MUCHOS_AUTOS + 1)
```

La única cuestión a tener en cuenta aquí es que está bueno que los tests tengan **la mínima lógica posible**, de manera de no estar repitiendo la misma lógica que ya tiene el negocio: la ventaja que tiene escribir `crearFlota(6)` es que si el límite de lo que se considera muchos autos cambia, el test falla y eso puede ser útil.

> Una heurística posible sobre el setup del test es tratar de mantenerlo simple y de alto nivel, más cercano al lenguaje del dominio que con detalles de implementación. En el ejemplo de arriba se logra con mensajes que se encargan de instanciar objetos de dominio y que esconden la complejidad de conocer la colaboración entre la flota y sus autos). Una alternativa a tener métodos en el test puede ser crear un objeto específico que construya otro objeto, algo que dejaremos para más adelante.

<br>

### Act

**A**ct: son las operaciones que tienen efecto. En el caso de la flota que tiene una deuda abultada, enviamos el mensaje que le genera la deuda. Hay tests que quizás no necesiten disparar acciones, y está bien que eso ocurra.

### Assert

En el **A**ssert indicamos qué esperamos que pase, generalmente asociado a las respuestas que da el envío de un mensaje al objeto testeado. Para esto utilizamos los [matchers](https://kotest.io/docs/assertions/matchers.html) de Kotest.

### "One assert per test"

Hay ciertas controversias respecto a si [podemos tener varios asserts en el mismo test](https://softwareengineering.stackexchange.com/questions/7823/is-it-ok-to-have-multiple-asserts-in-a-single-unit-test), ya que cuando el primer assert falla los siguientes no se siguen evaluando: esto en realidad depende del __runner__ de los tests, podríamos eventualmente trabajar con un framework que continue buscando asserts y discrimine cuáles anduvieron y cuáles no (RSpec, framework de testeo para Ruby, hace ésto). 

En verdad, la heurística que nos interesa recomendar es: **los tests deben fallar por exactamente un solo motivo**, esto relaja esa restricción. Lo importante no es tener un solo assert, sino que todos los asserts estén relacionados con la misma funcionalidad. Dejamos un ejemplo concreto:

```java
describe("Dado un parser de patentes de autos") {
	it("se obtiene correctamente la parte numérica de una patente vieja") {
		val lista = PatenteParser("ABC257").parsearNumeros()
		lista.size shouldBe 3
		lista[0] shouldBe 2
		lista[1] shouldBe 5
		lista[2] shouldBe 7
	}
}
```

El lector puede profundizar con estos artículos:

- [Multiple Asserts Are OK](https://www.industriallogic.com/blog/multiple-asserts-are-ok/)
- [Good Unit Test - One Assert](https://mfranc.com/unit-testing/good-unit-test-one-assert/)

## TL;DR

Este es el resumen de buenas prácticas a la hora de definir tus tests:

- armá los escenarios que definen las clases de equivalencia de los tests
- escribí la descripción de los describes y los tests explicando **qué** estamos testeando. El cómo lo terminás de ver en el código, **evitá duplicidades** entre el texto que explica y el código escrito
- separá los describes por requerimientos / casos de uso y los tests por escenarios donde quede claro la clase de equivalencia (cliente común, flota, etc.)
- los nombres de las variables deben reflejar la clase de equivalencia que están resolviendo, y no casos particulares que no revelan la intención de lo que estamos modelando (sí `flotaConPocosAutos`, no `flotinha` o `miFlota`)
- los tests se suelen estructurar utilizando las tres A: Arrange (el setup que conviene mantenerlo simple), Act (operaciones con efecto cuando corresponde) y Assert (las aserciones que deben testear el mismo concepto en cada test)

## Links relacionados

- [Video en youtube que explica cobertura y clases de equivalencia en un proyecto Kotlin](https://youtu.be/7rw3SNVI5yY)
- Si conocés Ruby, te recomendamos [Better specs](http://www.betterspecs.org/)
- [Página principal de Algoritmos 2](algo2-temario.html)
