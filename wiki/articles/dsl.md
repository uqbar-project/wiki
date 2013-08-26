DSL viene de las siglas en inglés **Domain-Specific Language**.

Un DSL es un lenguaje ideado para expresar cierta parte de un sistema. Por eso se dice que es un lenguaje de propósito específico, a diferencia de los lenguajes de propósito general (GPL).

Qué quiere decir esto ?

|                    | propósito general                                    | propósito específico       |
|--------------------|------------------------------------------------------|----------------------------|
| Abarca construir   | la totalidad de la aplicación                        | una parte de la aplicación |
| Tipo de Aplicacíón | cualquiera                                           | un solo tipo               |
| Conceptos          | generales:                                           
                                                                            
                      -   función (en funcional),                           
                      -   clase, objeto, método (en OOP),                   
                      -   variable, predicado (en lógico)...                | del (único) dominio        |
| Ejemplos           | C, Java, Smalltalk, Self, ADA, Haskell, Prolog, etc. | xpath, SQL, pic, sed       |

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

DSL Interno
-----------

DSL Externo
-----------
