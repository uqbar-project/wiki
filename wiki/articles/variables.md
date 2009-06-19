Muchachos, no olvidar: en objetos una variable es una **referencia** a un objeto. Cuando una variable nace referencia al objeto especial `nil`.

La asignación en objetos debe leerse así

`  variable := expresion-que-devuelve-un-objeto`

cuando se evalúa esta línea, la variable referencia al objeto resultado de la expresión.

Entonces, al asignar una variable **no estoy creando ningún objeto**, hago una referencia de una variable a un objeto existente. En las líneas del estilo

`  unaVar := UnaClase new.`

pasan **dos** cosas, en el orden que se indica

1.  se crea un objeto instancia de la clase UnaClase.
2.  se hace que la variable unaVar haga referencia al objeto recién creado.

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

Comentarios

-   en las líneas 2 y 3 se crean dos objetos (un jugador y una carta) sin que se asignen a ninguna variable.

