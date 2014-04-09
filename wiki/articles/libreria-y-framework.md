Librería
--------

Una **librería** es un conjunto de funciones llamadas desde "afuera" por un cliente. Dentro del POO, esto es una clase que recibe un mensaje, lo ejecuta y devuelve luego el control al cliente. La instanciación de una librería es relativamente sencilla.

![](Libreria.png "Libreria.png")

Framework
---------

El **framework** representa una abstracción de diseño y tiene un comportamiento en sí mismo. No es solamente una clase, sino que es un conjunto de objetos que se relacionan para servir a un dominio específico. El cliente puede usar el framework subclasificando o componiendo sus propias clases con las clases del framework y entonces el código del framework es el que llama al código cliente. La instanciación del framework no es tan sencilla, ya que requiere un conocimiento del mismo.

![](Framework.png "Framework.png")

Diferencias entre librería y framework
--------------------------------------

Mientras que los frameworks trabajan en un dominio concreto (testing, persistencia, exportación a pdf, etc.) los patterns son soluciones generales independientes del dominio que necesitan aplicarse en un contexto para poder ser implementados.
