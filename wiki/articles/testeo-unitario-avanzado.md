---
layout: article
title: Testeo unitario avanzado
---


## Testeo unitario avanzado

Este artículo presenta algunas guías para desarrollar los casos de prueba, considerando que ya tienen una base de testeo unitario automatizado. Los puntos a tener en cuenta son:

- elegir los escenarios posibles (las clases de equivalencia)
- para cada uno de los escenarios, considerar
  - el caso feliz que debe cumplir el sistema
  - los casos borde
  - los casos que el sistema debe rechazar


## Ejemplo

Un sistema de seguros de automotor define cuándo pagar un siniestro, las condiciones pueden variar:

- para los clientes normales, si no son morosos (la deuda debe ser 0)
- para la flota, se soporta una deuda de hasta $ 10.000 si la flota tiene más de 5 autos ó hasta $ 5.000 en caso contrario

## Definiendo los escenarios

En base al ejemplo anterior, podemos considerar los siguientes escenarios:

- un cliente normal moroso
- un cliente normal que debe $1 (caso borde) => está en la misma clase de equivalencia que el que debe más de $ 1
- una flota con 3 autos
- una flota con 6 autos
- adicionalmente, podemos pensar el caso borde, una flota con 5 autos

## Estructura de los tests

La estructura que tienen los tests en base a los escenarios propuestos podría ser:

```
dado un cliente normal
  ├── que es moroso: no puede cobrar un siniestro
  └── que no es moroso: puede cobrar un siniestro
dado un cliente de flota con muchos autos (supongamos 6, que es más de 5)
  ├── si el cliente debe más de $ 10.000 no puede cobrar un siniestro
  ├── si el cliente debe $ 10.000 puede cobrar un siniestro 
  └── si el cliente debe menos de $ 10.000, puede cobrar un siniestro
dado un cliente de flota con pocos autos (supongamos 3, que es menos de 5)
  ├── si el cliente debe más de $ 5.000 no puede cobrar un siniestro
  ├── si el cliente debe $ 5.000 puede cobrar un siniestro (caso borde)
  └── si el cliente debe menos de $ 5.000 puede cobrar un siniestro
dado un cliente de flota con 5 autos (caso borde)
  └── si el cliente debe $ 5.001 no puede cobrar un siniestro
```

## Definiendo las clases y las variables de los tests

Necesitamos

- un cliente normal
- una flota de 6 autos
- otra flota de 5 autos
- otra flota de 3 autos

a los que podemos configurar diferentes grados de deuda. Podemos seguir algunas recomendaciones adicionales:

### Agrupar los escenarios en clases

¿Cuántas clases necesitamos para implementar los casos de prueba? Podríamos considerar una clase sola para todos los tests, o bien tener dos clases: una para clientes normales y otra para clientes de flota, o bien podríamos tener una clase para cada uno de los escenarios que planteamos más arriba (cliente normal moroso, cliente que no debe nada, flota de 6 autos, etc.)

<!-- -->

Tengamos en consideración que tener en una sola clase todos los tests no resulta ser una buena práctica, porque

- dificulta diferenciar los escenarios, están todas las variables mezcladas
- cada vez que corre el setup debemos instanciar un cliente normal moroso, otro que no debe nada, una flota con 6 autos, otra con 5 y otra con 3, penalizando así a los tests que se concentran en una sola de estas clases de equivalencia
- la clase a testear pierde cohesión, está cubriendo todos los casos de prueba

<!-- -->
<br/>
Volviendo al ejemplo, hay varias opciones posibles:

- tener una clase para clientes normales y otra para clientes de flota
- tener una clase para clientes normales, y una clase para cada una de las 3 clases de equivalencia de flota (muchos autos, pocos autos, caso borde con 5 autos)

<!-- -->
<br/>
Crearemos entonces cuatro clases de test:

- ClienteNormalTest
- FlotaPocosAutosTest
- FlotaMuchosAutosTest
- FlotaCasoBordeTest

Es importante que no haya demasiados detalles de implementación en los nombres: FlotaCon3AutosTest o FlotaCon5AutosTest está sujeto a que cualquier cambio del negocio respecto a lo que son "muchos" o "pocos" autos necesite modificar el nombre de la clase.

### Intention revealing - parte 1

Queremos expresar lo más claramente posible la intención de la clase: qué clase de equivalencia está testeando. El nombre ayuda, pero JUnit 5 nos permite incorporar la anotación `@DisplayName`:

```java
@DisplayName("Dado un cliente de flota con muchos autos")
class FlotaMuchosAutosTest {
```

recordando que las clases agrupan los tests en forma jerárquica, más adelante veremos cómo juega a favor este encabezado escrito en lenguaje natural.

## Expresividad en los tests

### Un primer approach

Para crear nuestro fixture de una flota con muchos autos, los enunciados suelen traer ejemplos: "Lidia Pereyra tiene una flota con 6 autos". Podríamos sentirnos tentados a escribir un test como el siguiente:

```java
	Flota pereyra
	
	@BeforeEach
	def void init() {
		pereyra = new Flota => [
			agregarAuto(new Auto("ab028122", 2008))
      // ... se agregan más autos ... //
		]
	}

	@Test
	def void pereyraNoPuedeCobrarSiniestro() {
		pereyra.generarDeuda(15000)
		assertFalse(pereyra.puedeCobrarSiniestro)
	}
```

Pero ¿qué pasa si hay un error en el código de negocio? Supongamos esta implementación, donde la clase Cliente tiene la definición de la deuda como un entero:

```java
class Flota extends Cliente {
	List<Auto> autos = newArrayList
	
	override puedeCobrarSiniestro() {
		this.deuda < this.montoMaximoDeuda
	}
	
	def montoMaximoDeuda() {
		if (autos.size > 5) 20000 else 5000 // debería ser 10.000 y no 20.000
	}
```

Cuando ejecutamos el test tenemos muy poca información relevante:

![mal nombre de variable](/img/wiki/testeo_mal_nombre_variable.png)

- la variable `pereyra` no está revelando que es un cliente de flota con muchos autos
- y tampoco está claro por qué no puede cobrar el siniestro el cliente. 

Al fallar la condición tenemos que bucear en el código y extraer este dato para determinar si el error está en el test o en el código de negocio. 

### Una segunda oportunidad

Vamos a mejorar la semántica del test, renombrando la variable `pereyra` por un nombre más representativo de la clase de equivalencia que estamos modelando, agregando la anotación `@DisplayName` para el test y definiendo un mensaje de error adicional en el assert:

```java
class FlotaMuchosAutosTest {
	
	Flota flotaConMuchosAutos
	
	@BeforeEach
	def void init() {
		flotaConMuchosAutos = new Flota => [
			agregarAuto(new Auto("ab028122", 2008))
      // ... agregamos más autos ... //
		]
	}

	@Test
	@DisplayName("si tiene una deuda grande no puede cobrar un siniestro")
	def void pereyraNoPuedeCobrarSiniestro() {
		flotaConMuchosAutos.generarDeuda(15000)
		assertFalse(flotaConMuchosAutos.puedeCobrarSiniestro, 
      "una flota que tiene una deuda abultada no puede cobrar un siniestro")
	}
```

Ahora al fallar el test sabemos más cosas:

![mas expresividad en los tests](/img/wiki/testeo_mas_expresivo.png)

- el test con su stack trace, pero también
- qué es lo que estamos testeando, tratando de no entrar en detalles para no duplicar lo que dice el código
- qué se esperaba que pasara y no pasó, en un formato legible para un usuario: "Dado un cliente de flota con muchos autos, si tiene una deuda grande no puede cobrar un siniestro"

<!-- -->
<br/>

## AAA Pattern

Los tests suelen estructurarse según el patrón AAA: Arrange, Act y Assert.

- **A**rrange: donde instanciamos los objetos a testear, con sus colaboradores: en el ejemplo son la flota y sus autos. Cuando los contextos son compartidos, los frameworks basados en xUnit (JUnit5 es uno de ellos) nos permiten ubicarlo en un método `setup` (`@BeforeEach`). La desventaja de esta técnica es que para tener una idea general de los elementos que participan en el test debemos mirar el test y el setup, por eso una alternativa suele ser tener objetos específicos que se encargan de crear una flota, separados del test:

```java
	@Test
	def void sinDeudaPuedeCobrarSiniestro() {
		val flotaConMuchosAutos = FlotaFactory.crearFlotaConAutos(6) // cantidad de autos
		assertTrue(flotaConMuchosAutos.puedeCobrarSiniestro)
	}
```

En el ejemplo tenemos un objeto FlotaFactory que permite crear un objeto Flota pasándole la cantidad de autos a crear. De esa manera la configuración de una flota ocurre en una sola línea y se puede incluir dentro del test mismo.

- **A**ct: son las operaciones que tienen efecto. En el caso de la flota que tiene una deuda abultada, enviamos el mensaje que le genera la deuda.

- **A**ssert: qué esperamos que pase, generalmente asociado a las respuestas que da el envío de un mensaje al objeto testeado.

### "One assert per test"

Hay ciertas controversias respecto a si [podemos tener varios asserts en el mismo test](https://softwareengineering.stackexchange.com/questions/7823/is-it-ok-to-have-multiple-asserts-in-a-single-unit-test), ya que cuando el primer assert falla los siguientes no se siguen evaluando. En realidad, si seguimos la recomendación de que **los tests deben fallar por exactamente un solo motivo**, esto relaja esa restricción. Es decir, lo importante no es tener un solo assert, sino que todos los asserts estén relacionados con la misma funcionalidad. El lector puede profundizar con estos artículos:

- [Multiple Asserts Are OK](https://www.industriallogic.com/blog/multiple-asserts-are-ok/)
- [Good Unit Test - One Assert](https://mfranc.com/unit-testing/good-unit-test-one-assert/)

## TL;DR

Este es el resumen de buenas prácticas a la hora de definir tus tests:

- armá los escenarios que generalmente definen las clases de tests
- utilizá anotaciones `@DisplayName` para la clase de test y para cada test, de manera de entender **qué** estamos testeando. El cómo se ve en el código, **evitá duplicidades** entre el texto que explica y el código escrito
- evitá que una clase de test tenga muchos escenarios juntos, es más difícil seguirlos (por ese motivo preferimos no explicar el anidamiento de tests en JUnit5)
- los nombres de las variables deben reflejar la clase de equivalencia que están resolviendo, y no casos particulares que no revelan la intención de lo que estamos modelando (sí `flotaConPocosAutos`, no `flotinha` o `miFlota`)
- los tests se suelen estructurar utilizando las tres A: Arrange (el setup), Act (operaciones con efecto cuando corresponde) y Assert (las aserciones que deben testear el mismo concepto en cada test)

## Links relacionados

- Si venís del mundo de Ruby, te recomendamos [Better specs](http://www.betterspecs.org/)
- [Página principal de Algoritmos 2](algo2-temario.html)
