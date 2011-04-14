La posibilidad de definir funciones utilizando aplicación parcial en la propia definición trae aparejado a veces la dificultad para entender cuántos parámetros espera esa función que se está definiendo.

Por ejemplo la función es múltiplo así definida tiene dos parámetros:

esMultiploDe a
==============

(==0).( \`mod\` 5)

Una regla más o menos intuitiva es que una función así definida tiene tantos parámetros como la suma entre: - Los parámetros que aparecen a la izquierda del igual (en este caso 1 parámetro: a). - Los parámetros que "le faltan" a la expresión a la derecha del igual (en este caso eso es una composición, por lo tanto una función que recibe un parámetro, es decir "le falta" un parámetro. = 2 parámetros.

Fíjense que en la forma más tradicional de escribir funciones, a la expresión a la derecha del igual no le falta ningún parámetro:

siguiente x = x + 1

Se ve claramente que x+1 es una expresión que denota un valor y no una función, no le faltan parámetros, como sí le faltarían si yo pongo (+1) o algo así.

Por eso esa expresión es equivalente a:

siguiente = (+1)
