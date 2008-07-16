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
`  cacho := Chofer new.`
`  "..."`
`  elReyDelAcceso := Camion new.`
`  elReyDelAcceso initialize.`
`  elReyDelAcceso conductor: cacho.`

Ahora bien, serían muy felices dos cosas

-   ahorrarme el decirle "inicialize" a cada Camion que creo
-   poder pasarle el chofer al camión en el momento de crearlo,

Una forma es la [lazy inicialization](lazy-inicialization.html), otra es bleh
