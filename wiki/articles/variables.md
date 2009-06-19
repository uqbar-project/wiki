Muchachos, no olvidar: en objetos una variable es una **referencia** a un objeto. Cuando una variable nace referencia al objeto especial `nil`.

La asignación en objetos debe leerse así

`  variable := expresion-que-devuelve-un-objeto`

cuando se evalúa esta línea, la variable referencia al objeto resultado de la expresión.

Entonces, al asignar una variable **no estoy creando ningún objeto**, hago una referencia de una variable a un objeto existente. En las líneas del estilo

`  unaVar := UnaClase new.`

pasan **dos** cosas, en el orden que se indica

1.  se crea un objeto instancia de la clase UnaClase.
2.  se hace que la variable unaVar haga referencia al objeto recién creado.

### Un ejemplito

Miremos esta secuencia de sentencias, se numeran para hablar de ellas abajo.

`  1. col1 := OrderedCollection new.`
`  2. col1 add: Jugador new.`
`  3. col1 add: Carta palo: 'espada' numero: 3.`
`  4. col1 add: 'hola'.`
`  5. jug := col1 first.`
`  6. tacar := col1 second.`
`  7. elPalo := var2 palo.`
`  8. jug agregarCarta: (Carta palo: 'oro' numero: 4).`
`  9. col2 := col1.`
` 10. col2 add: 38.`

Comentarios

-   en las líneas 2 y 3 se crean dos objetos (un jugador y una carta) sin que se asignen a ninguna variable.
-   en las líneas 5, 6 y 7 se asignan variables con referencias a objetos que no se crean en la misma línea, que "vienen desde antes".
-   en la línea 8, otra vez se crea un objeto sin asignarlo a una variable.
-   en la línea 9, las variables col2 y col1 pasan a apuntar al **mismo** objeto.
-   por lo tanto si después de la línea 10 pido `col1` `size` me va a decir 4.

