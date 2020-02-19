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

Vamos a mejorar la semántica del test, renombrando la variable `pereyra`, agregando la anotación `@DisplayName` para el test y definiendo un mensaje de error adicional en el assert:

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
- qué se esperaba que pasara y no pasó

<!-- -->
<br/>

## Links relacionados

- [Página principal de Algoritmos 2](algo2-temario.html)
