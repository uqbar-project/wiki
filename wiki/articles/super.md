**super** es una [pseudovariable](pseudovariable.html) muy parecida a [self](self.html).

**super** referencia (apunta) al objeto receptor del mensaje del método que estamos analizando en un momento dado al igual que **self**. La diferencia entre **self** y **super** es que **super** afecta al [ method lookup](paradigma-de-objetos---method-lookup.html)

Sin la existencia de super tendríamos problemas para resolver ciertos problemas, por ejemplo si tenemos la siguiente jerarquía de clases `A` `<|-` `B`:

`#A >> m`
`   "lógica definida por A para cuando se recibe el mensaje m"`
`   ^ 'hola'`
`#B >> m`
`   "queremos usar la lógica que define A y además hacer otra cosa"`
`   ^ `**`self` `m`**` , ' mundo'`

Si a una instancia de B le mandamos el mensaje m ejecutará el método m definido en B y en su definición se manda a sí mismo m, con lo cual se ejecuta ese mismo método (no el de A) y así indefinidamente. Para solucionar este problema podemos usar super.

`#A >> m`
`   "lógica definida por A para cuando se recibe el mensaje m"`
`   ^ 'hola'`
`#B >> m`
`   "queremos usar la lógica que define A y además hacer otra cosa"`
`   ^ `**`super` `m`**` , ' mundo'`

Cuando en un método se manda un mensaje a super el method lookup se ve modificado empezando la búsqueda del método correspondiente a ese mensaje en la clase inmediatamente superior a donde está definido ese método, en vez de en la clase de la cual es instancia el objeto. De esta forma podemos [redefinir](redefinicion.html) un método de la superclase para agregar lógica nueva y a su vez reutilizar el comportamiento heredado.

Supongamos que tenemos la siguiente jerarquía de clases `A` `<|-` `B` `<|-` `C` que definen:

`#A >> m`
`   ^ self n + 4`
`   >> n`
`   ^ 1`
`#B >> m`
`   ^ super m + 5`
`#C >> n`
`   ^ 3`

Sabiendo que tanto **self** como **super** apuntan a **i**, o sea, ambos apuntan al objeto receptor del mensaje, analicemos lo que pasa si evaluamos lo siguiente:

`i := C new.`
`i m.`

Cuando i recibe el mensaje m busca un método en su clase C con el mismo nombre y no lo encuentra, con lo cual lo sigue buscando en la superclase de C, que es B. Allí existe una definición para ejecutar y vemos que al objeto referenciado por **super** (que es i) se le manda el mensaje m, haciendo que el method lookup para ese mensaje comience en la clase A (superclase de donde está definido el método). La definición de \#A&gt;&gt;m envía el mensaje n al objeto referenciado por **self** (que también es i), pero el method lookup para encontrar el método n comienza desde la clase de la cual es instancia i (o sea en C).

El resultado final del cálculo sería: 3 + 4 + 5

Si le mandáramos el mensaje m a una instancia de B el resultado sería: 1 + 4 + 5, ya que se ejecutaría \#B&gt;&gt;m que hace `self` `n`, se empieza a buscar n en B, no se encuentra, se lo busca en A y retorna 1.

Conclusión

-   super sólo debe usarse para redefinir métodos que en su definición envían el mismo mensaje que se está definiendo para usar el código heredado evitando loops que se provocarían en el method lookup usando self
-   Si el receptor del mensaje **NO** es **super**, se busca en la clase de la cual es instancia el objeto un método con el mismo nombre del mensaje.
-   Si el receptor del mensaje ES **super** entonces se busca la definición en la superclase de la clase en donde está el método que contiene a la pseudovariable **super**.

Malos usos de super
-------------------

Supongamos que tenemos este código para la jerarquía `A` `<|-` `B`

`#A >> m1`
`  "hace algo"`
`#B >> m1`
`  super m1`

Esta redefinición usando super es innecesaria, ya que si a una instancia de B se le manda m1 el mismo method lookup buscará la definición en A en caso de no encontrarla en B.

Otro error muy común (y muy grave) es usar super para mandar un mensaje con nombre diferente al método que se está definiendo:

`#A >> n`
`   ^ 1`
`#B >> m`
`   ^ `**`super` `n`**` + 5`
`#C >> n`
`   ^ 3`

En este ejemplo se puede ver que se manda el mensaje n en B usando super en vez de self en el método m. Esto es un problema ya que se ignora el comportamiento que puedan tener para n las subclases de B así como una posible implementación futura de n para B. Este mal uso de super suele llevar a un comportamiento inesperado del sistema que puede no resultar en un error como sucede en este caso (retornaría 6 en vez de 8), con lo cual es muy difícil de detectar y corregir.
