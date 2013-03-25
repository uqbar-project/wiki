### Concepto Matemático

Las funciones partidas ó definidas por partes ó por trozos son funciones que para diferentes valores del dominio, tienen una definición diferente. Por ejemplo, si tenemos que definir la función Módulo, para ciertos valores del dominio la función será `f(x)` `=` `x` y para otros valores será `f(x)` `=` `-x`

#### Notación Matemática

![](modulo.gif "modulo.gif")

#### Código en Haskell

En Haskell, las funciones partidas se escriben con Guardas, y se escribe la imagen de cada parte *a la derecha* del igual:

`f x | x >= 0 = x`
`    | x <  0 = -x`

Las funciones por partes no son la única forma de tener diferentes definiciones para una función, existen casos en los cuales alcanza con el uso de [pattern matching](pattern-matching-en-haskell.html).

### Errores Comunes

No debemos confundir el uso de guardas (cuyo concepto se explicó arriba) con las funciones que devuelven booleanos.

#### Ejemplo 1

Tomemos éste ejemplo: "Cierta edad es adulta cuando es 18 ó más"

`esAdulta edad | edad >= 18 = True`
`              | otherwise = False`

¡Pero esto es un uso incorrecto de las guardas! En otras palabras: Si una expresión es verdadera ó falsa *por sí sola*, no se lo debo preguntar, tengo que devolver directamente eso.

Ésta es la **manera correcta** de hacerlo:

`esAdulta edad = edad >= 18`

#### Ejemplo 2

Representemos una función que me diga si alguien siempre dice la verdad, sabiendo que "los niños y los borrachos siempre dicen la verdad" Ésta es una manera de hacerlo, errónea:

`siempreDiceLaVerdad alguien | esNiño alguien = True`
`                            | esBorracho alguien = True`
`                            | otherwise = False`

Si bien ésto funciona, es un mal uso de las funciones por partes, ya que no es cierto que esa función tenga diferentes "partes". La definición es lógica, y es *una sola*. Es un "ó" lógico:

¿Cuando alguien dice la verdad?

-   cuando es niño
-   ó
-   cuando es borracho

*Esa es la definición de la función*. Recordemos que el **ó** logico se escribe así:

`Main> (1>34) || (even 4)`
`True`

y el **y** lógico se escribe así:

`Main> (1>34) && (even 4)`
`False`

Entonces, mi función se define **sin guardas**, de ésta manera:

`siempreDiceLaVerdad alguien = esNiño alguien || esBorracho alguien`
