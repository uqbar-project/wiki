---
layout: article
title: Dsl
---

DSL viene de las siglas en inglés **Domain-Specific Language**.

Un DSL es un lenguaje ideado para expresar cierta parte de un sistema. Por eso se dice que es un lenguaje de propósito específico, a diferencia de los lenguajes de propósito general (GPL).

Los lenguajes con los que estamos acostumbrados a trabajar, como Java, Haskell o Groovy tienen la característica de que pueden ser utilizados para resolver problemas de programación de cualquier índole, por lo que decimos que son lenguajes de propósito general (GPL por sus siglas en inglés).

Estos lenguajes son Turing-complete, siendo capaces de expresar cualquier algoritmo, y pudiendo ser aplicados a cualquier dominio. Con estos lenguajes, con mayor o menor facilidad o eficiencia, podemos construir sistemas de cálculo impositivo, implementar algoritmos de aprendizaje de máquina, etc. Es decir, son lenguajes que pretenden ser igualmente efectivos (igualmente buenos o malos) en todos los campos de accion.

Sin embargo, el empleo GPLs para expresar problemas muy específicos, si bien es posible, puede significar mayor esfuerzo de lo que uno desearía, dado que las abstracciones que estos dominios plantean no están soportadas nativamente por el lenguaje; no son primitivas. Por ejemplo, escribir una transposición de matrices, aún contando con API bien diseñada, es ciertamente más complejo en Java que en Mathematica u Octave, y lenguajes como SQL están específicamente diseñados para realizar operaciones sobre una base datos.

Estos lenguajes son específicos de un dominio particular (DSL, domain-specific languages), y si bien no pueden resolver todos los problemas, resuelven aquellos para los que fueron diseñados mejor que los GPL.

Qué quiere decir esto ?

|                    | propósito general                                 | propósito específico                                                                                                                                                                                   |
|--------------------|---------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Abarca construir   | la totalidad de la aplicación                     | una parte de la aplicación                                                                                                                                                                             |
| Tipo de Aplicacíón | cualquiera                                        | un solo tipo                                                                                                                                                                                           |
| Conceptos          | generales:                                        
                                                                         
                      -   función (en funcional),                        
                      -   clase, objeto, método (en OOP),                
                      -   variable, predicado (en lógico)...             | del (único) dominio                                                                                                                                                                                    |
| Ejemplos           | C, Java, Smalltalk, Self, ADA, Haskell, Prolog... | [xpath](http://es.wikipedia.org/wiki/XPath), SQL, [pic](http://macroexpand.org/doku.php?id=articles:uml-sequence-diagram-dsl-txl:start), [sed](http://es.wikipedia.org/wiki/Sed_(inform%C3%A1tica))... |

Problema que ataca un DSL
-------------------------

En general cuando nos enfrentamos a un problema de programación aparecen varias actividades que trataremos de simplificar ampliamente acá:

1.  Entendimiento del problema/dominio
2.  Formación conceptual de una solución
3.  Implementación en un lenguaje de programación
4.  Compilación, ejecución y pruebas.

Normalmente trabajamos con un único paradigma de programación y unos pocos lenguajes. Estos lenguajes permiten expresar sobre ellos cualquier tipo de dominio, es decir que se utilizan para desarrollar cualquier tipo de aplicación. Por esto es que se llaman **lenguajes de propósito general (GPL del inglés)**.

En el proceso de **entendimiento del dominio** podríamos trabajar completamente abstractos del lenguaje de programación, simplemente tratando de relevar información y requerimientos. No vamos a entrar en detalles acá acerca de esta etapa, pero lo que nos interesa es que **se podría hacer una análisis del problema independiente del lenguaje de programación y hasta del paradigma**.

Durante la formación conceptual de una solución ya debemos pensar el dominio dentro de uno o varios paradigmas (en caso en que osemos desarrollar la solución con múltiples paradigmas), de acuerdo a nuestra experiencia, o lo que veamos que mejor se adapte a la problemática.

Por ejemplo, hay problemas que son inherentemente más fáciles de implementar en el paradigma lógico que en objetos, o funcional, etc. Sin embargo, todavía podríamos pensar en una solución independiente del lenguaje.

Por ejemplo, en objetos es donde identificamos:

-   objetos y clases.
-   responsabilidades (mensajes).
-   interacciones y colaboraciones.
-   jerarquías / traits (lógica común y reutilización.
-   etc.

Luego tenemos que realmente implementar esta solución abstracta en los **pasos 3 y 4**, y para eso utilizamos un GPL.

Y acá va el problema:

-   El mapeo de la solución conceptual con el lenguaje no es directo, no suele ser trivial.
-   A veces no tenemos soporte del lenguaje para ciertas abstracciones de nuestro dominio: Los ejemplos más simples son los patrones como singleton, delegación automática, etc.
-   Estamos forzados a adaptar el dominio y nuestra solución al lenguaje.

Y eso es lo que hacemos siempre, adaptamos al lenguaje que tenemos "a mano". Eso nos lleva a que nuestra solución va a estar siempre de aquí en adelante expresada en un lenguaje que no es el más cercano al problema/dominio, sino más bien a un lenguaje de programación general.

Algunas consecuencias:

**Legibilidad**:

-   Nuestro código contendrá una mezcla entre conceptos de dominio (una Cuenta, un Cliente, etc) y palabras propias del lenguajes (keywords, como public class, trait, etc).
-   Quien se incorpore al proyecto o quiera revisar la solución deberá, naturalmente, hacer el proceso inverso, como una **ingeniería reversa**, a partir del código y de lo expresado en el GPL abstraerse para generar una representación mental del problema/dominio.

**Flexibilidad** (cambios de requerimientos o dominio):

-   Naturalmente solo podrán ser implementados por desarrolladores que entiendan no solo el dominio, sino además el GPL.
-   Cada nuevo cambio deberá ser traducido nuevamente **de dominio hacia GPL**.

**Duplicación**:

-   Tendremos al menos dos representaciones de la solución, la mental (que muchas veces además se plasma en documentos de especificación y análisis) y la traducción/implementación en el GPL.

Estas consecuencias hacen que la programación dedique **la mayor parte del tiempo y esfuerzo en lidiar con los problemas de traducción e implementación de la solución al GPL**.

Entonces, una vía alternativa sería en pensar que **en lugar de adaptar nuestra solución a un lenguaje, podemos adaptar el lenguaje a nuestra solución**.

A esto se lo conoce como **[Language-Oriented Programming](http://www.onboard.jetbrains.com/articles/04/10/lop/2.html)**, desde el punto de vista de un nuevo "paradigma". Y una de las prácticas es crear un nuevo lenguaje para expresar nuestra solución o una parte de la solución. Esto es un **DSL**.

Motivación para hacer un DSL
----------------------------

Suponemos que con la comparación que ya vimos, aparecen varias razones. Pero vamos a enumerarlas acá para resumir:

-   para acercar la brecha entre la descripción del problema en términos abstractos (descripción del dominio) y la implementación de la "computación" que lo resuelve / ejecuta.
    -   Facilitaría la comunicación con gente no-técnica.
    -   No programadores podrían entenderlo y escribirlo.
-   para esconder los detalles internos
    -   de la lógica común
    -   o las construcciones propias del lenguaje
-   para configuraciones de ciertas partes de la aplicación.
-   para expresar de forma declarativa ciertas reglas del negocio, que se va a separar de la forma en que se interprete y ejecute esa regla. Ej: regular expressions.
-   para la creación de un grafo complejo de objetos, problema que normalmente solucionamos con patrones creacionales, como factories, builders & prototypes.

¿Cuando necesito un DSL? Como aproximación, diremos que si la cantidad de problemas en este domino específico que queremos resolver es pequeño, o la complejidad de crear el DSL es mayor que la resolver el problema en nuestro GPL, probablemente no lo necesitemos. De lo contrario, será una alternativa a considerar.

Tipos de DSL's
--------------

A grandes rasgos, podríamos catalogar los DSLs a través de las siguientes categorías. Existen autores que refinan mucho más a detalle la categorización, incluyendo nuevos tipos. A fines prácticos de explicar la idea general a nivel de programación, nosotros optamos por acotar esa categorización:

-   Parser + Compilador ó Interprete
    -   Compilador: traduce a lenguaje maquina ejecutable: puede ser assembler o un lenguaje ejecutable por una VM como java, etc.
    -   Interprete: a medida que se va parseando (leyendo) el código de expresado en el DSL, se va ejecutando, sin haber "generado" código ejecutable como paso intermedio.
-   Traductor:
    -   son los famosos generadores de código.
    -   si bien se podría trazar una analogía con los compiladores, ya que también generan código, la diferencia es que los traductores generan código que no es "ejecutable" de por sí, sino más bien código de otro lenguaje.
    -   Ejemplo: generadores de código. a través de Java APT (annotation processing tool)
-   Embebidos:
    -   se utiliza un lenguaje GPL, pero se lo usa de tal manera de "simular" o asemejarse lo mayor posible a un lenguaje propio del dominio
    -   con syntax sugar y un diseño de API's especial, llamado Fluent Interfaces, se acerca de un lenguaje de dominio.
    -   aprovechar las características del lenguaje GPL existente,
    -   Evita tener que hacer un parser, compilador e interprete.
    -   Ej: ruby

### DSL Interno

Desarrollados como un API dentro de un lenguaje de proposito general, que se ejecutan en su mismo ambiente, llamado lenguaje huésped.

Si comparamos los DSLs internos y externos, en general los segundos serán mas flexibles, pero también el esfuerzo de implementarlos será mayor, no sólo porque la complejidad de implementar un parser a mano es mayor, sino que probablemente el lenguaje huésped nos proveerá muchas construcciones y bibliotecas útiles. (Digresión: aprovechar las bibliotecas existentes para otro lenguaje es un punto muy importante también a la hora de diseñar un GPL. Así es como surge gran cantidad de lenguajes que corren sobre la máquina virtual de Java o .Net)

[Ejemplo de DSL interno para definir especificaciones](http://utntadp.com.ar/cursadas-anteriores/2012c1-clases/clase-12#TOC-Spec) aprovechando [features de lenguajes dinámicos](uso-de-features-de-lenguajes-dinamicos.html)

### DSL Externo

Desarrollados como un lenguaje independiente, compilado, interpretado o una mezcla de ambos, que se ejecuta en un ambiente independiente, y no guarda necesariamente relación con el lenguaje en que está escrito el parser.

**Ventajas**

-   Libertad absoluta sobre la sintaxis del lenguaje (solo limitada por capacidad de implementación del parser, etc), por eso..
-   Mayor expresividad del dominio.

**Desventajas**

-   Complejidad al tener que implementar el parser + compiler
-   Disasociación del lenguaje "base" o de ejecución.
-   Caemos fuera de las herramientas tradicionales, y perdemos soporte a nivel IDE (salvo, ahora con xtext)

[Artículo sobre cómo hacer un DSL externo usando XText](http://www.drdobbs.com/open-source/project-of-the-month-xtext-dsl-framework/230700063)
