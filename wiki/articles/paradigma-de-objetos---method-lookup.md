*También conocido como **Dynamic dispatch***

Es el mecanismo por el cual se relaciona el envío de un mensaje con la ejecución de un método determinado.

En Smalltalk usando clases

¿Qué pasa cuando a un objeto **i** instancia de la clase **C** se le envía un mensaje de nombre **m**?

Tenemos que mantener una referencia a la clase donde estamos buscando en un momento determinado. Al principio la *clase actual* es **C**

El algoritmo es el siguiente:

**1.** se búsca en la *clase actual* un método con el nombre **m**

**1a.** si se encuentra se ejecuta el método encontrado; se ejecuta el método en el objeto **i** y se terminó el method lookup

**1b.** si no se encuentra y la *clase actual* no es **Object** la *clase actual* pasa a ser la superclase de la *clase actual* y se vuelve a **1.**

**1c.** si no se encuentra y la clase actual es **Object** entonces el objeto **i** no entiende el mensaje **m**

El comportamiento por defecto en Smalltalk cuando un objeto no entiende un mensaje es lanzar un error. Esto se logra a través del envío de un mensaje llamado **\#doesNotUnderstand:** al objeto **i** (el método **\#doesNotUnderstand:** está definido en la clase **Object**)
