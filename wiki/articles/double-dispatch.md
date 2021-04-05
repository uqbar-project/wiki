---
layout: article
title: Double dispatch
---

Double dispatch es un patrón de diseño que permite de tomar una decision a partir de varios objetos en vez de uno solo.
En las implementaciones de lenguajes de objetos mainstream, la decisión de qué método se va a ejecutar se hace a partir del objeto receptor del mensaje.
Sin embargo, a veces con el objeto receptor no alcanza.

Para dar un ejemplo trivial pero ilustrativo, en el juego de piedra, papel, tijera, la decision de quien gana y quien pierde depende de dos objetos y no de uno solo.
En dicho caso, una solución posible es que uno de los dos objetos delegue la decisión al segundo, dándole información de sí mismo. En este caso, el primer objeto va a avisarle al segundo objeto quién es (en este caso una piedra), y el segundo objeto va a decidir quien gana sabiendo eso: otra piedra va a decidir empate, una tijera que gana la piedra, y un papel que gana el papel.

```smalltalk
Piedra >> quienGanaContra: otro
  ^ otro quienGanaContraPiedra: self

Piedra >> quienGanaContraPiedra: unaPiedra
  ^ nil
  
Papel >> quienGanaContraPiedra: unaPiedra
  ^ self
  
Tijera >> quienGanaContraPiedra: unaPiedra
  ^ unaPiedra

...
```

Una definición posible de Double Dispatch, es la propuesta por [Ralph Johnson](http://www.laputan.org/reflection/Foote-Johnson-Noble-ECOOP-2005.html).

### ¿Qué criterio de dispatch utilizar?

Johnson entiende que el Double Dispatch siempre va a sociado a la clase del segundo objeto; si bien comprende eso como un [Code Smell](code-smell.html) lo asume inevitable y característico del Double Dispatch.

Sin embargo, otra forma de interpretarlo sería que el mejor Double Dispatch es el que logra evitar el code smell y en lugar de realizar el dispatch sobre el tipo del parámetro lo hace en función al rol que ocupa, es decir, dándole significado más allá del *tipo* en sí. Por supuesto este tipo de double dispatch requiere de un diseño un poco más, pero es esperable que sea más extensible: si el dispatch se realiza por el tipo tenemos dos desventajas claras:

-   Explosión combinatoria (como explica Johnson si tenemos 6 subtipos diferentes podemos tener 6x6 combinaciones de dispatchs).
-   No extensible, ya que agregar un nuevo tipo implicaría necesariamente agregar todas las combinaciones posibles.

Esto no descarta la posibilidad de realizar un double dispatch basado en tipos, pero considera que el code smell no puede ser ignorado y por lo se prefiere evitar esa forma de utilización. En una estrategia de dispatch basada en el comportamiento de los objetos, es más probable que los nuevos casos que surjan puedan asociarse a alguno de los roles preexistentes.

### ¿Qué usos tiene?

Unos de los usos principales del double dispatch son los patrones de diseño de objetos visitor e interpreter.
El patrón visitor sirve para hacer extensible una estructura de datos, modelando operaciones sobre la estructura como objetos "visitantes".
Los usuarios pueden definir nuevos visitantes, y cada visitante sabe como tiene que manipular cada objeto en la estructura de datos usando el double dispatch.

Otro posible uso es para implementar el patron interpreter, un patron usado para evaluar sentencias de un programa.
En el patron interpreter, un programa se representa como un árbol, sobre el que uno puede implementar el patron visitor, donde un objeto intérprete juegue el rol de visitante.