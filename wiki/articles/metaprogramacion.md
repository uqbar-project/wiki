**Metaprogramación** es el proceso o la práctica por la cual escribimos programas que generan, manipulan o utilizan otros programas.

Ejemplos:

-   Un compilador se puede pensar como un programa que genera otro programa.
-   Un formateador de código es un programa que manipula otro programa.
-   Una herramienta como javadoc utiliza nuestro programa para generar su documentación.

Para qué se usa la metaprogramación ?
-------------------------------------

En general la metaprogramación se utiliza más fuertemente en el **desarrollo de frameworks**. Simplemente porque un framework va a resolver cierta problemática de una aplicación, pero no va a estar diseñado para ninguna en particular. Es decir, la idea de framework es que se va a poder aplicar y utilizar en diferentes dominios desconocidos para el creador del framework.

Entonces estos frameworks van a manipular objetos, sin conocerlos de antemano.

Ejemplos:

-   ORM's como hibernate: que van a encargarse de persistir las instancias de nuestras clases sin siquiera conocerlas de antemano.
-   Frameworks de UI: que deberán saber mostras cualquier objeto.
-   Otras herramientas:
    -   javadoc: es una herramienta como el compilador de java, que lee el código fuente y genera documentación html.
    -   code coverage: herramientas que miden cuánto de nuestro código es realmente ejecutado al correr los tests, y cuales lineas no.
    -   analizadores de código: que evalúan nuestro código y genera métrics o miden violaciones a reglas definidas. Como el estilo de código, complejidad ciclomática, etc. Por ejemplo para java existe sonar que junto a maven automatizan y concrentran varias otras herramientas.

Modelos y metamodelos
---------------------

Así como todo programa construye un modelo para describir su dominio. El domino de un metaprograma es otro programa denominado programa objeto o base y tendrá un modelo que describe a ese programa, al que llamamos **metamodelo**.

En el siguiente ejemplo, nuestro dominio contiene diferentes tipos de animales, entre ellos perros y humanos.

El programa describe las características de los elementos del dominio utilizando (por ejemplo) clases, métodos y atributos. Entonces, el modelo contiene una clase Perro, que modela a los perros en el domino. Y el programa manipula instancias de la clase Perro.

Un metaprograma tendrá a su vez un (meta)modelo que describe a su dominio, el programa base. Así como en el dominio hay animales concretos, los habitantes del "metadominio" (= programa base) serán los elementos del programa: por ejemplo, clases, atributos, métodos. Entonces el metamodelo deberá tener clases que permitan describir esos conceptos, por ejemplo en el metamodelo de Java encontraremos las clases Class, Method, Field. Este metamodelo describe la estructura posible de un programa Java. En otro lenguaje, ese metamodelo tendría diferentes elementos.

Así como el programa manipula las instancias de las clases Perro o Animal, el metaprograma manipula las instancias de las clases que conforman el metamodelo (Class, Method, Field, o las que fueran).

Para poder trabajar con el metamodelo hay que usar las herramientas de [Reflection](reflection.html) que el lenguaje nos brinde.
