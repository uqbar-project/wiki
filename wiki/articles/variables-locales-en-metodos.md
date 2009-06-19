Gente, no olviden (¡no olviden!) que pueden usar variables locales a métodos. Son análogas a las variables locales de procedimientos que deberían conocer de Algoritmos.

Cómo se definen y usan: ahí va un ejemplo

` miMetodo`
`   | varLocal1 varLocal2 varLocal3 |`
`   varLocal1 := UnaClase new.`
`   varLocal2 := varLocal1 unMensaje.`
`   varLocal3 := 4.`
`   varLocal1 mensajeConParametro: varLocal2.`
`   "etc"`

En este método se definen tres variables locales. Las primeras tres líneas asignan las tres variables (o sea, hacen que referencien a un objeto); fíjense que en la segunda le estoy enviando un mensaje al objeto referenciado por varLocal1.
