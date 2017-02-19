---
layout: article
title: Macros en Scala
categories: [scala]
---

# Introducción

Macros es una herramienta muy poderosa que permite definir reescrituras de AST (Abstract Syntax Tree) y está presente en 
muchos lenguajes y tecnologías. A grandes razgos, la útilidad de las macros consiste en tomar una construcción sintáctica 
válida y reemplazarla por otra en tiempo de compilación, permitiendo así que la sintaxis que normalmente construiría un 
cierto programa construya otro totalmente diferente. En Scala, la utilización de macros está definida en el paquete

```scala
scala.language.experimental.macros
```

el cual debe ser importado para poder trabajar.

Una macro de Scala se compone de dos partes: Una declaración y una implementación. Al momento de compilar, 
los usos de la función declaración son procesados para reemplazarlos por el resultado de aplicar la función implementación. 
Definir la declaración de una macro es muy similar a definir una función común pero, en lugar del cuerpo, se utiliza la 
palabra clave macro seguida del nombre de la función implementación.

```scala 
  // declaración
  def miMacro(parametro1: String, parametro2: Int) = macro miMacro_impl
  
  // implementación
  def miMacro_impl(c: Context)(parametro1: c.Expr[String], parametro2: c.Expr[Int]) = ???
```
# Whitebox y Blackbox macros

Existen 2 tipos de macros, las llamadas “de caja blanca” o whitebox y las “de caja negra” o blackbox. La diferencia entre 
los dos enfoques es que las macros de caja negra se usan cuando puedo definir claramente una firma para la función que quiero
implementar usando macros, mientras que las de caja blanca se usan cuando no puedo definir dicha firma. 
Las macros de caja blanca son más flexibles pero menos seguras, ya que no pueden tiparse y van a ser discontinuadas en 
versiones futuras de Scala, por esa razón vamos a concentrarnos en las definiciones de caja negra.

Para elegir uno de estos dos enfoques es necesario importar el paquete correspondiente

```scala 
import scala.reflect.macros.whitebox
```
para las de caja blanca y

```scala 
import scala.reflect.macros.blackbox
```

para las de caja negra.

La clase Context que se usó en el código anterior está definida en estos paquetes.

Mirando el ejemplo, se puede ver que hay una relación entre el tipo de la declaración de la macro y su implementación que, 
además de recibir un parámetro Context, espera también un parámetro por cada parámetro de la declaración que debe tener 
el mismo nombre y un tipo de expresión que coincida. creado a partir del contexto.


# Usos

Veremos que hay varias maneras de colgarse del proceso de compilador, por lo que tenemos distintos tipos de macros 
propuestos por scala, solo que en este caso nos estaremos enfocando en uno de los tipos de macros. Otra consideración a tener en cuenta es que la interfaz que tenemos de macros como la de reflection en scala puede ir variando en el tiempo, ya que son aún implementaciones experimentales y no se ha llegado a un estado final de como sería la implementación definitiva.

Las macros han sido utilizados durante la versión 2.10 de Scala, tanto para aplicaciones de investigación como industriales, 
y la conclusión según {% cite Burmako_scalamacros %}, es que las macros han sido útiles para aplicaciones tales como:

- Code Generation
- Implementation of DSLs
- Static checking among others

# Mas información 

Ver más en detalle sobre macros en scala 2.10 en el siguiente [link](http://tadp-utn-frba.github.io/scripts/clase_14)


{% include article_bibliography.html %}