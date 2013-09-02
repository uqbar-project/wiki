Un prototipo según la RAE es un "Ejemplar original o primer molde en que se fabrica una figura u otra cosa." Este término se utiliza en la programación orientada a objetos para aquellos objetos que sirven como base para obtener otros objetos como él, la forma de obtener nuevos objetos a partir del prototipo se denomina **clonación**.

¿Qué **problema** es el que resuelve la clonación, entonces?

Si ya existe un objeto con un determinado comportamiento y estado interno y lo clonamos, obtendremos otro objeto con el mismo comportamiento (entiende los mismos mensajes y se definen de la misma forma) y estado interno (comparte la forma, pero cada objeto tiene sus propias referencias, ya que queremos que el estado de cada objeto sea independiente) que el original.

¿Qué significa ésto? Que si queremos que todas las golondrinas del universo coman y vuelen de la misma forma (pero que cada una tenga su propia energía), entonces podemos hacer que todas sean clones de "pepita", nuestra golondrina original, la que usaremos como prototipo.

La principal diferencia entre clonar un objeto y crear otro copiando el código que ya teníamos es que el código no está duplicado. Al decir que un objeto es clon de otro estamos estableciendo una relación fuerte entre ellos; si se agrega, quita o modifica algún método en el prototipo, todos sus clones se verán afectados por este cambio, lo cual lógicamente no sucede si duplicamos código, tendríamos que hacer los cambios en cada lugar que esté repetido pudiendo olvidar hacer alguno o cometer errores fácilmente. Lo mismo sucederá si le agregamos o quitamos alguna referencia al prototipo, el mismo cambio se hará sobre sus clones. Donde apunte la referencia en cada clon será probablemente diferente.

Volviendo sobre el ejemplo de pepita la golondrina, supongamos que queremos clonarla para crear otra golondrina, josefa.

Si teníamos el siguiente código en el objeto pepita:

`#pepita`
`>> vola: unosKilometros`
`  "Gasta 5 joules por kilómetro más 10 de despegue"`
`  energia := energia - (unosKilometros * 5 + 10)`

Y le mandamos a josefa

`josefa vola: 10.`

El objeto josefa buscará la implementación de vola: en sí mismo inicialmente, y al no tener una definición propia, la seguirá buscando en su prototipo que es pepita. Allí encuentra una definición y la ejecuta. La energía que va a disminuir es la de josefa, el estado interno de pepita se va a mantener intacto.

También podríamos tener una definición de vola: usando mensajes a [self](self---pseudovariable.html) en vez de accediendo directamente a la variable energía:

`#pepita`
`>> vola: unosKilometros`
`  self energia: self energia - (unosKilometros * 5 + 10)`

Como self siempre es el objeto receptor del mensaje, si vola: se lo mandamos a **josefa**, self estará referenciando a josefa, no a pepita.

Lógicamente, si le mandamos vola: a pepita, va a entender el mensaje, a ejecutar su propia definición de vola: y modificar su propia energia sin afectar a ninguno de sus clones.

Una vez que tenemos el objeto clonado podemos trabajar con él modificando su comportamiento y su estado interno, con lo cual podremos mantener las similitudes con su prototipo en aquellos puntos en los cuales nos interese e introducir diferencias en otros. Si el prototipo define un método para el mensaje \#msj1 y el clon cambia su implementación, el prototipo no se verá afectado por este cambio y el clon usará su propia implementación cuando reciba \#msj1 en vez de usar la del prototipo.

A modo resumen podemos decir que el [method lookup](method-lookup.html) al usar prototipado se resuelve de la siguiente forma:

Tenemos el objeto o1 cuyo prototipo es o2, y o1 recibe el mensaje \#m

-   o1 tiene un método cuyo nombre es \#m, se ejecuta ese método.
-   o1 no tiene un método cuyo nombre es \#m, entonces busca una definición en su prototipo
    -   o2 tiene un método cuyo nombre es \#m, se ejecuta ese método (el receptor sigue siendo o1)
    -   o2 no tiene un método cuyo nombre es \#m, con lo cual habrá un error porque o1 no entiende el mensaje \#m

Si o2 a su vez tuviera otro prototipo, el method lookup continuaría hasta que se llegue a un objeto que no es clon de ningún otro.

Para descargar la herramienta que soporta prototipado en Pharo Smalltalk y tutoriales de uso: [Object Browser (LOOP)](https://sites.google.com/site/objectbrowsertool/)
