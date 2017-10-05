---
layout: article
title: Super
---

**super** es una [pseudovariable](pseudovariable.html) muy parecida a [self](self.html).

**super** referencia (apunta) al objeto receptor del mensaje del método que estamos analizando en un momento dado al igual que **self**. La diferencia entre **self** y **super** es que **super** afecta al [ method lookup](paradigma-de-objetos---method-lookup.html)

Problema que resuelve
---------------------

Sin la existencia de super tendríamos problemas para resolver ciertos problemas, por ejemplo si tenemos la siguiente jerarquía de clases `A` `<|-` `B`:

```Smalltalk
#A >> m
   "lógica definida por A para cuando se recibe el mensaje m"
   ^ 'hola'
#B >> m
   "queremos usar la lógica que define A y además hacer otra cosa"
   ^ self m , ' mundo'
```

Si a una instancia de B le mandamos el mensaje m ejecutará el método m definido en B y en su definición se manda a sí mismo m, con lo cual se ejecuta ese mismo método (no el de A) y así indefinidamente. Para solucionar este problema podemos usar super.

```Smalltalk
#A >> m
   "lógica definida por A para cuando se recibe el mensaje m"
   ^ 'hola'
#B >> m
   "queremos usar la lógica que define A y además hacer otra cosa"
   ^ super m , ' mundo'
```

Cuando en un método se manda un mensaje a super el method lookup se ve modificado empezando la búsqueda del método correspondiente a ese mensaje en la clase inmediatamente superior a donde está definido ese método, en vez de en la clase de la cual es instancia el objeto. De esta forma podemos [redefinir](redefinicion.html) un método de la superclase para agregar lógica nueva y a su vez reutilizar el comportamiento heredado.

Supongamos que tenemos la siguiente jerarquía de clases `A` `<|-` `B` `<|-` `C` que definen:

```Smalltalk
#A >> m
   ^ self n + 4
   >> n
   ^ 1
#B >> m
   ^ super m + 5
#C >> n
   ^ 3
```

Sabiendo que tanto **self** como **super** apuntan a **i**, o sea, ambos apuntan al objeto receptor del mensaje, analicemos lo que pasa si evaluamos lo siguiente:

```Smalltalk
i := C new.
i m.
```

Cuando i recibe el mensaje m busca un método en su clase C con el mismo nombre y no lo encuentra, con lo cual lo sigue buscando en la superclase de C, que es B. Allí existe una definición para ejecutar y vemos que al objeto referenciado por **super** (que es i) se le manda el mensaje m, haciendo que el method lookup para ese mensaje comience en la clase A (superclase de donde está definido el método). La definición de \#A&gt;&gt;m envía el mensaje n al objeto referenciado por **self** (que también es i), pero el method lookup para encontrar el método n comienza desde la clase de la cual es instancia i (o sea en C).

El resultado final del cálculo sería: 3 + 4 + 5

Si le mandáramos el mensaje m a una instancia de B el resultado sería: 1 + 4 + 5, ya que se ejecutaría \#B&gt;&gt;m que hace `self` `n`, se empieza a buscar n en B, no se encuentra, se lo busca en A y retorna 1.

Resumen
-------

-   Si el receptor del mensaje **NO** es **super**, se busca en la clase de la cual es instancia el objeto un método con el mismo nombre del mensaje.
-   Si el receptor del mensaje ES **super** entonces se busca la definición en la superclase de la clase en donde está el método que contiene a la pseudovariable **super**.

Conclusión
----------

*super **sólo** debe usarse para **redefinir** métodos que envían el **mismo mensaje** que se está definiendo, para **evitar loops** que se provocarían en el method lookup usando self*

Malos usos de super
-------------------

### Caso 1

Supongamos que tenemos este código para la jerarquía `A` `<|-` `B` (O sea B es subclase de A)

Código erróneo:

```Smalltalk
#A >> m1
  "hace algo"
#B >> m1
  super m1
```

Esta redefinición usando super es innecesaria, ya que si a una instancia de B se le manda m1 el mismo method lookup buscará la definición en A en caso de no encontrarla en B. Es además incorrecta, porque al redefinir un método semánticamente estoy indicando que "hago las cosas distintas a arriba", cuando éste no es el caso.

Código correcto:

```Smalltalk
#A >> m1
  "hace algo"
```

¡No hace falta escribir nada, B ya entiende el mensaje m1!

### Caso 2

Otro error muy común (y muy grave) es usar super para mandar un mensaje con nombre diferente al método que se está definiendo: (C hereda de B, B hereda de A)

Código erróneo:

```Smalltalk
#A >> m1
   ^ 1
#B >> m2
   ^ super m1 + 5
#C >> m1
   ^ 3
```

En este ejemplo se puede ver que se manda el mensaje m1 en B usando super en vez de self en el método m2. Esto es un problema ya que se ignora el comportamiento que puedan tener para m1 las subclases de B así como una posible implementación futura de m1 para B. Este mal uso de super suele llevar a un comportamiento inesperado del sistema que puede no resultar en un error como sucede en este caso (retornaría 6 en vez de 8), con lo cual es muy difícil de detectar y corregir.

Código correcto:

```Smalltalk
#A >> m1
   ^ 1
#B >> m2
   ^ self m1 + 5
#C >> m1
   ^ 3
```

¡Si yo soy un B, quiero hacer m1 como un B! Siempre voy a querer comportarme como me corresponde a mí. Que ese método esté definido en la superclase es *una coincidencia*. Si yo quiero hacer m1, entonces hago self m1. (Releer la conclusión)
