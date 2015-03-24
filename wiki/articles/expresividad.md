### Definición

La expresividad puede definirse informalmente con la heurística "el nivel de **lindez** del código". En otras palabras, escribir un código **expresivo** es poner atención a las cuestiones que hacen que este código fuente sea más fácil de *entender* por *una persona*.

*¿Por qué por una persona y no por una pc?*

Para responder esa pregunta comparemos estos dos códigos en Pascal:

`PROGRAM `

En el código de la derecha puede verse con claridad el objetivo de este programa, mientras que en el de la izquierda está "escondido". Sin embargo, la computadora ejecutando este código produce exactamente el mismo resultado con cualquiera de los dos programas. La diferencia está en el programador que lee un programa ó el otro.

Es por eso que muchas veces se suele considerar a la Expresividad como algo **subjetivo**. Sin embargo, en líneas generales, hay formas de alcanzar la expresividad.

### Motivación

En general, las técnicas que favorecen la *mejor comprensión* del código fuente (por un programador) son técnicas que *no cambian en funcionamiento del programa*. Entonces, si en última instancia el programa hace lo que corresponde, ¿Por qué habríamos de consumir tiempo escribiendo código expresivo?

En la industria actual de software (de hecho, en cualquier ambiente en el que sea necesaria la producción de software) existen ciertas características / problemas a resolver, consecuencia de que *los programas son cada vez más grandes, complejos, y cambiantes*. En consecuencia:

`* Se espera que sean flexibles (que puedan cambiarse fácilmente)`
`* Se espera que "fallen poco" (con lo cual es importantísimo encontrar y corregir errores tempranamente).`
`* El desarrollo dura mucho tiempo. (Meses, años)`
`* El equipo de desarrollo es amplio. (Mucha gente escribiendo el mismo programa).`

En consecuencia, la labor de un programador es en su amplia mayoría, leer y corregir código existente (propio ó de otro) y en menor medida producir código nuevo.

Es por todo esto que el código fuente *no puede ser exclusivamente escrito para la computadora*. El más importante destino del código son las propias personas.

=== Cóo lograr la
