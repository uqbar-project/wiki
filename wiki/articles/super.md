**super** es una [pseudovariable](pseudovariable.html) muy parecida a [self](self.html).

**super** referencia (apunta) al objeto receptor del mensaje del método que estamos analizando en un momento dado.

La diferencia entre **self** y **super** es que **super** afecta el [ method lookup](paradigma-de-objetos---method-lookup.html)

Cuando super está presente el algoritmo se ve modificado

Tenemos un objeto **i** instancia de la clase **C** y le enviamos un mensaje de nombre **m**.

En este ejemplo self empieza apuntando a **i** y super también apunta a **i**

Tenemos que mantener una referencia a la clase donde estamos buscando en un momento determinado.

Al principio la *clase actual* es **C**

**1.**

si el receptor del mensaje **NO** es **super** se búsca en la *clase actual* un método con el nombre **m**

si el receptor del mensaje ES **super** entonces *clase actual* pasa a ser la superclase de la clase en donde está el método que contiene a la pseudovariable **super**

**1a.** si se encuentra se ejecuta el método encontrado; se ejecuta el método en el objeto **i** y se terminó el method lookup

**1b.** si no se encuentra y la *clase actual* no es **Object** la *clase actual* pasa a ser la superclase de la *clase actual* y se vuelve a **1.**

**1c.** si no se encuentra y la clase actual es **Object** entonces el objeto **i** no entiende el mensaje **m**
