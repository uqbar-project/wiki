Una definición posible de Double Dispatch, es la propuesta por [Ralph Johnson](http://www.cincomsmalltalk.com/userblogs/ralph/blogView?showComments=true&printTitle=Double_dispatch&entry=3485318158).

### ¿Qué criterio de dispatch utilizar?

Sin embargo, Johnson entiende que el Double Dispatch siempre va a sociado a la clase del segundo objeto; si bien comprende eso como un [Code Smell](code-smell.html) lo asume inevitable y característico del Double Dispatch.

Otra forma de interpretarlo sería que el mejor Double Dispatch es el que logra evitar el code smell y en lugar de realizar el dispatch sobre el tipo del parámetro lo hace en función al rol que ocupa, es decir, dándole significado más allá del *tipo* en sí. Por supuesto este tipo de double dispatch requiere de un diseño un poco más, pero es esperable que sea más extensible: si el dispatch se realiza por el tipo tenemos dos desventajas claras:

-   Explosión combinatoria (como explica Johnson si tenemos 6 subtipos diferentes podemos tener 6x6 combinaciones de dispatchs).
-   No extensible, ya que agregar un nuevo tipo implicaría necesariamente agregar todas las combinaciones posibles.

En una estrategia de dispatch basada en el comportamiento de los objetos, es más probable que los nuevos casos que surjan puedan asociarse a alguno de los roles preexistentes.
