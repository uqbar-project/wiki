A veces, es necesario inicializar alguna/s de la/s variable/s de un objeto, con objetos fijos, o con objetos que hay que pasarle apenas se crea el objeto.

Para esto hacemos un método al que por convención se lo llama initialize, y le enviamos el mensaje correspondiente a cada objeto que creamos desde el workspace. Por ejemplo

`  #Camion`
`  initialize`
`     viajes := Set new.`
`     desgasteDeLasRuedas := 0.`
`     puedeViajar := true.`
`  chofer := unFercho`
`     chofer := unFercho`

`  "... en el workspace ..."`
`  elReyDelAcceso := Camion new.`
`  elReyDelAcceso initialize.`
`  elReyDelAcceso conductor: cacho.`
`  "cacho lo tenía creado más arriba en el workspace"`

Ahora bien, serían muy felices dos cosas

-   ahorrarme el decirle "inicialize" a cada Camion que creo
-   poder pasarle el chofer al camión en el momento de crearlo,

Con la [lazy inicialization](lazy-inicialization.html), puedo lograr lo primero pero no lo segundo.

Una forma de lograr las dos cosas es definir un método de clase que tiene como objetivo crear un objeto ya configurado y listo para usar.

¿Cómo hacemos? Fácil

`  #Camion class`
`  nuevoConducidoPor: unFercho`
`      | camionNuevo |`
`      camionNuevo := self new.`
`      camionNuevo initialize.`
`      camionNuevo chofer: unFercho.`
`      ^camionNuevo`

Vemos que el método

-   crea un camión nuevo enviándose un mensaje a self, que es la clase Camion.
-   lo configura
-   **lo devuelve** (muuuuy importante)

En el workspace nos queda

`  elReyDelAcceso := Camion nuevoConducidoPor: cacho.`

¿qué ganamos?

-   que no nos olvidamos de inicializar ningún camión
-   que el workspace queda más compacto, importante para los workspace de TP que pueden implicar la creación de muchos objetos.
-   que si le digo a la gente que va a usar los camiones que los cree siempre diciéndole a la clase lo que yo les indico, todos los camiones van a nacer ya con chofer asignado, lo que tal vez es necesario para que los camiones no den errores después.

