*También conocido como **Dynamic dispatch***

Es el mecanismo por el cual se relaciona el envío de un mensaje con la ejecución de un método determinado.

En Smalltalk usando clases

Cuando a una instancia **x** de la clase **X** se le envía un mensaje de nombre **m** pasa lo siguiente

Tenemos que mantener una referencia a la clase donde estamos buscando en un momento determinado, al principio la *clase actual* es **X**

El algoritmo es el siguiente

1. se búsca en la *clase actual* un método con el nombre **m** 1a. si se encuentra se ejecuta el método encontrado, se ejecuta en el objeto **x** y se terminó el method lookup 1b. si no se encuentra y la *clase actual* no es **Object** la *clase actual* pasa a ser la superclase de la *clase actual* y se vuelve a 1. 1c. si no se encuentra y la clase actual es Object entonces la instancia **x** no entiende el mensaje **m**
