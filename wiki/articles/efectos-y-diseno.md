Entendemos por efectos (colaterales) a aquellas modificaciones observables sobre el ambiente que son producidas por, y persisten tras, la ejecución de un programa.

Son ejemplos de efectos:

-   la impresión de un mensaje por consola o un cuadro de diálogo en una interfaz gráfica
-   la asignación destructiva de una variable en un programa procedural
-   el encendido de una lámpara o la apertura de una compuerta
-   La eliminación de un registro en una base de datos
-   El envío de un correo electrónico.

En un sentido amplio, no es posible eliminar completamente los efectos: un sistema (software o no) que no presente efecto no será capaz de transformar su entorno, y por lo tanto, será inútil completamente: por ejemplo de nada sirve generar un informe sobre contaminación del agua si luego este no es tomado en cuenta en los controles.

Por el contrario, los sistemas reales modifican de forma observable su mundo, recopilando información y actuando.

Al hablar de efectos, es importante remarcar la importancia del observador, el nivel de detalle y alcance al que estamos analizando el problema. Por ejemplo, la impresión en papel de un informe, desde el punto de vista de la cola de impresión es una operación con efecto, pero desde el punto de vista de un gerente, no lo tiene en tanto no llegue a sus manos y se tomen acciones en base a su contenido. Desde el punto de vista del microprocesador, una simple suma tiene efecto en tanto implica modificaciones en sus registros.

Aquí solo consideraremos como efectos aquellos que sean observables por o mas allá de nuestro código, dejando fuera todo aquel efecto completamente encapsulado en el motor, como la Máquina Virtual de Java, o el intérprete GHC

La programación OO tradicional permite y fomenta el uso de efectos. Los objetos responden a mensajes, eventualmente produciendo efectos, y son responsables tanto de generar como controlar estos efectos ordenando su ejecución y protegiendo su estado interno.

La programación funcional en su forma más básica, en cambio, se trata de resolver los problemas en términos de aplicación de funciones puras, es decir, desprovistas de efecto. Entonces, si decimos que los efectos son necesarios, ¿que beneficio nos puede reportar la programación funcional?

En primer lugar, porque que los programas con efectos son más difíciles de desarrollar y probar: los efectos no se ven en el código y no pueden ser trazados facilmente, a diferencia de un valor etiquetado, que es fácil de encontrar y predecir su comportamiento independientemente del lugar donde se encuentre. En un programa puro, si la expresión:

`objeto.mensaje(argumento) `
`resulta en la evaluación de x(objeto, argumento), entonces siempre la primera expresión puede ser reemplazada por la segunda (transparencia referencial). Como caso particular, a iguales argumentos, igual resultado. Además asegura que la evaluación de cualquier otra expresión no habrá sido modificada por esta. Ninguna de ambas afirmaciones son verdaderas en un programa con efectos. `

Diseñar un programa con efectos agrega entradas y salidas omnipresentes e invisibles; los datos viajan de forma global y las transformaciones ocurren en el tiempo. Su manejo se vuelve crítico cuando nuestro código se ejecuta en múltiples hilos, o ante la presencia de excepciones.

Ejemplos

` Afortunadamente, aún es posible diseñar programas o partes de programas desprovistos de efectos, como parte de sistemas mayores: una calculadora, un compilador, un motor de reglas de un filtro de correo, son algunos de los infinitos programas que pueden ser modelados sin efecto. `

`En OO existe una tendencia tendencia natural pero errada de forzar las responsabilidades y relaciones entre los objetos imiten a las del mundo real, perdiendo de vista que el objetivo de un sistema oo normalmente no se trata de simular sino resolver problemas, y en ocasiones las soluciones que se apartan de la realidad son igualmente simples de entender, y más fáciles de implementar y mantener. Esta malinterpretación lleva a que los objetos también presenten cambios de estado superficiales, aun cuando, como señalábamos antes, podrían haber realizado la misma tarea sin éste. `

La programación funcional entonces nos permite mejorar nuestros diseños de objetos, al eliminar los efectos innecesarios.

En segundo lugar, porque aún en el uso de los efectos, es posible y en ocaciones desaeable un tratamiento a más alto nivel del los mismos. Una de las fortalezas de los objetos es su capacidad, comparada con la programación procedural, para la construcción efectos controlados y ordenados pero si bien triunfa al controlar la generación de efectos, falla al intentar separar su generación de su evaluación, al cual ocurre indefectiblemente en el mismo momento. La programación funcional nuevamente nos provee estrategias para cosificar (reificar) a los efectos, transformándolos en valores, y dandole el poder de controlar los efectos a incluso a aquellos objetos que no lo generaron.

snd (putStr "", putStr "foo")

Por último, una consecuencia interesante del asilamiento de los efectos es que conlleva una modularización del código
