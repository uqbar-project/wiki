*También conocido como **Dynamic dispatch***

Es el mecanismo por el cual se relaciona el envío de un mensaje con la ejecución de un método determinado.

En Smalltalk usando clases
--------------------------

¿Qué pasa cuando a un objeto **i** instancia de la clase **C** se le envía un mensaje de nombre **m**?

Tenemos que mantener una referencia a la clase donde estamos buscando en un momento determinado.

Al principio la *clase actual* es **C** y el objeto receptor del mensaje ([self](self---pseudovariable.html)) es **i**.

El algoritmo es el siguiente:

**1.** se busca en la *clase actual* un método con el nombre **m**

**1a.** si se encuentra se ejecuta el método encontrado; se ejecuta el método en el objeto **i** y se terminó el method lookup

**1b.** si no se encuentra y la *clase actual* no es **Object** la *clase actual* pasa a ser la superclase de la *clase actual* y se vuelve a **1.**

**1c.** si no se encuentra y la clase actual es **Object** entonces el objeto **i** no entiende el mensaje **m**

El comportamiento por defecto en Smalltalk cuando un objeto no entiende un mensaje es lanzar un error. Esto se logra a través del envío de un mensaje llamado **\#doesNotUnderstand:** al objeto **i** (el método **\#doesNotUnderstand:** está definido en la clase **Object**)

Ejemplo 1
---------

Si le enviamos el mensaje **asUppercase** al objeto `'hola'` (o sea, al string `'hola'`) ¿qué debería pasar?

Asumamos (porque no es así) que `'hola'` es instancia de **String**, que **String** es subclase de **Collection** y que **Collection** es subclase de **Object**.

Siguiendo el algorítmo de arriba a través de los pasos indicados con **-**

**i** es `'hola'`

*clase actual* es **String**

**m** es **\#asUppercase**

**-** se busca en **String** un método con el nombre **\#isNil**

**-** se encuentra el método **\#asUppercase** en la clase **String**

**-** se ejecuta el método **\#asUppercase** de la clase **String** en el objeto receptor del mensaje **i** (o sea `hola`)

Conclusión: `'hola'` entiende el mensaje **\#asUppercase**

Ejemplo 2
---------

Basandonos en el ejemplo 1, si le enviamos el mensaje **isNil** al objeto `'hola'` (o sea, al string `'hola'`) ¿qué debería pasar?

**i** es `'hola'`

*clase actual* es **String**

**m** es **\#isNil**

**-** se busca en **String** un método con el nombre **\#isNil**

**-** no se encuentra el método **\#isNil** en **String** y la *clase actual* no es **Object** entonces la *clase actual* pasa a ser **Collection** (la superclase de la *clase actual*) y se vuelve a **1.**

**-** se busca en **Collection** un método con el nombre **\#isNil**

**-** no se encuentra el método **\#isNil** en **Collection** y la *clase actual* no es **Object** entonces la *clase actual* pasa a ser **Object** (la superclase de la *clase actual*) y se vuelve a **1.**

**-** se busca en **Object** un método con el nombre **\#isNil**

**-** se encuentra el método **\#isNil** en la clase **Object**

**-** se ejecuta el método **\#isNil** de la clase **Object** en el objeto receptor del mensaje **i** (o seam `hola`)

Conclusión: `'hola'` entiende el mensaje **\#isNil**

Ejemplo 3
---------

Basandonos en el ejemplo 1, si le enviamos el mensaje **factorial** al objeto `'hola'` (o sea, al string `'hola'`) ¿qué debería pasar?

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
