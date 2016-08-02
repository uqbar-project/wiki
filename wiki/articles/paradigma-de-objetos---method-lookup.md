*También conocido como **Dynamic dispatch***

Es el mecanismo por el cual se relaciona el envío de un mensaje con la ejecución de un método determinado.

Con objetos individuales
------------------------

Cuando definimos un objeto y le declaramos su propio comportamiento, este mecanismo es trivial. Si existe un método que se llame igual que el mensaje definido y con la misma cantidad de parámetros, se ejecutará ese método, de lo contrario tirará un error porque no entiende el mensaje (a menos que se trate de un mensaje básico como la igualdad o identidad que cualquier objeto entiende).

Con clases y herencia simple
----------------------------

¿Qué pasa cuando a un objeto **i** instancia de la clase **C** se le envía un mensaje de nombre **m**?

Tenemos (a efectos de entender el mecanismo porque esto lo hace internamente el ambiente) que mantener una referencia a la clase donde estamos buscando en un momento determinado.

Al principio la *clase actual* es **C** y el objeto receptor del mensaje ([self](self---pseudovariable.html)) es **i**.

El algoritmo es el siguiente:

**1.** se busca en la *clase actual* un método con el nombre **m**

**1a.** si se encuentra se ejecuta el método encontrado; se ejecuta el método en el objeto **i** y se terminó el method lookup

**1b.** si no se encuentra y la *clase actual* no es **Object** la *clase actual* pasa a ser la superclase de la *clase actual* y se vuelve a **1.**

**1c.** si no se encuentra y la clase actual es **Object** entonces el objeto **i** no entiende el mensaje **m**

El comportamiento por defecto cuando un objeto no entiende un mensaje es lanzar un error.

Ejemplo 1
---------

Si le enviamos el mensaje **asUppercase** al objeto `'hola'` (o sea, al string `'hola'`) ¿qué debería pasar?

Asumamos (porque no es así) que `'hola'` es instancia de **String**, que **String** es subclase de **Collection** y que **Collection** es subclase de **Object**.

Siguiendo el algoritmo de arriba a través de los pasos indicados con **-**

**i** es `'hola'`

*clase actual* es **String**

**m** es **\#asUppercase**

**-** se busca en **String** un método con el nombre **\#asUppercase**

**-** se encuentra el método **\#asUppercase** en la clase **String**

**-** se ejecuta el método **\#asUppercase** de la clase **String** sobre **i** el objeto receptor del mensaje (o sea `hola`)

Conclusión: `'hola'` entiende el mensaje **\#asUppercase**

Ejemplo (heredando un método)
-----------------------------

Basándonos en el ejemplo 1, si le enviamos el mensaje **==** al objeto `'hola'` (o sea, al string `'hola'`) ¿qué debería pasar?

**i** es `'hola'`

*clase actual* es **String**

**m** es **\#==**

**-** se busca en **String** un método con el nombre **\#==**

**-** no se encuentra el método **\#==** en **String** y la *clase actual* no es **Object** entonces la *clase actual* pasa a ser **Collection** (la superclase de la *clase actual*) y se vuelve a **1.**

**-** se busca en **Collection** un método con el nombre **\#==**

**-** no se encuentra el método **\#==** en **Collection** y la *clase actual* no es **Object** entonces la *clase actual* pasa a ser **Object** (la superclase de la *clase actual*) y se vuelve a **1.**

**-** se busca en **Object** un método con el nombre **\#==**

**-** se encuentra el método **\#==** en la clase **Object**

**-** se ejecuta el método **\#==** de la clase **Object** sobre **i**, el objeto receptor del mensaje (o sea `hola`)

Conclusión: `'hola'` entiende el mensaje **\#==**

Ejemplo (no entiende el mensaje)
--------------------------------

Basándonos en el ejemplo 1, si le enviamos el mensaje **factorial** al objeto `'hola'` (o sea, al string `'hola'`) ¿qué debería pasar?

**i** es `'hola'`

*clase actual* es **String**

**m** es **\#factorial**

**-** se busca en **String** un método con el nombre **\#factorial**

**-** no se encuentra el método **\#factorial** en **String** y la *clase actual* no es **Object** entonces la *clase actual* pasa a ser **Collection** (la superclase de la *clase actual*) y se vuelve a **1.**

**-** se busca en **Collection** un método con el nombre **\#factorial**

**-** no se encuentra el método **\#factorial** en **Collection** y la *clase actual* no es **Object** entonces la *clase actual* pasa a ser **Object** (la superclase de la *clase actual*) y se vuelve a **1.**

**-** se busca en **Object** un método con el nombre **\#factorial**

**-** no se encuentra y la clase actual es **Object** entonces el objeto **i** no entiende el mensaje **m**

O dicho de otra forma, `'hola'` no entiende el mensaje **\#factorial**
