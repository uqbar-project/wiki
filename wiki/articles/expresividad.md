### Definición

La expresividad puede definirse informalmente con la heurística "el nivel de **lindez** del código". En otras palabras, escribir un código **expresivo** es poner atención a las cuestiones que hacen que este código fuente sea más fácil de *entender* por *una persona*.

*¿Por qué por una persona y no por una pc?*

Para responder esa pregunta comparemos estos dos códigos en Pascal:

<table>
<tbody>
<tr class="odd">
<td><p><code></p>
<p><code>Function QuieroMoverElBote(a: Array of Integer, c:Integer):Real;</code><br />
<code>Var b  : Integer; d  : Real; e  : Integer; Begin</code><br />
<code> For b := 1 to c do Begin e := e + a[b]; </code><br />
<code>  End;  d := e / c;</code><br />
<code> QuieroMoverElBote := d; End.</code></p>
<p></code></p></td>
<td><p>| <code></p>
<p><code>Function Promedio(numeros: Array of Integer, cantidad:Integer):Real;</code><br />
<code>Var</code><br />
<code>   i  : Integer;</code><br />
<code>   sumatoria  : Integer;</code><br />
<code>Begin</code><br />
<code> For i := 1 to cantidad do</code><br />
<code>  Begin</code><br />
<code>   sumatoria := sumatoria + numeros[i]; </code><br />
<code>  End;</code><br />
<code> Promedio := sumatoria / cantidad;</code><br />
<code>End.</code></p>
<p></code></p></td>
</tr>
</tbody>
</table>

En el código de la derecha puede verse con claridad el objetivo de este programa, mientras que en el de la izquierda está "escondido". Sin embargo, la computadora ejecutando este código produce exactamente el mismo resultado con cualquiera de los dos programas. La diferencia está en el programador que lee un programa ó el otro.

Es por eso que muchas veces se suele considerar a la Expresividad como algo **subjetivo**. Sin embargo, en líneas generales, hay formas de alcanzar la expresividad.

### Motivación

En general, las técnicas que favorecen la *mejor comprensión* del código fuente (por un programador) son técnicas que *no cambian en funcionamiento del programa*. Entonces, si en última instancia el programa hace lo que corresponde, ¿Por qué habríamos de consumir tiempo escribiendo código expresivo?

En la industria actual de software (de hecho, en cualquier ambiente en el que sea necesaria la producción de software) existen ciertas características / problemas a resolver, consecuencia de que *los programas son cada vez más grandes, complejos, y cambiantes*. En consecuencia:

-   Se espera que sean flexibles (que puedan cambiarse fácilmente)
-   Se espera que "fallen poco" (con lo cual es importantísimo encontrar y corregir errores tempranamente).
-   El desarrollo dura mucho tiempo. (Meses, años)
-   El equipo de desarrollo es amplio. (Mucha gente escribiendo el mismo programa).

En consecuencia, la labor de un programador es en su amplia mayoría, leer y corregir código existente (propio ó de otro) y en menor medida producir código nuevo.

Es por todo esto que el código fuente *no puede ser exclusivamente escrito para la computadora*. El más importante destino del código son las propias personas. Es por eso que no se puede descuidar la expresividad: es una de las varias formas de hacer la vida del programador más sencilla, para que pueda abordar la construcción de sistemas como el mencionado.

### Cómo lograr la expresividad

Hay varias formas de lograr expresividad, entre las que se destacan:

-   Usar buenos nombres
-   Usar buenas [abstracciones](abstraccion.html) en general (y en particular, la [Declaratividad](declaratividad.html)).
-   Identar correctamente el código.

#### Buenos Nombres

Buen resumen (en inglés) <http://c2.com/cgi/wiki?GoodVariableNames>

-   Un buen nombre debe decir exactamente cuál es el **propósito** de la variable / procedimiento / método.
    -   Por ejemplo, la función de arriba pasó de llamarse *QuieroMoverElBote* a *Promedio*, y el array de llamarse *a* a llamarse *numeros*.
-   Ser **descriptivo**.
    -   Por ejemplo, no tener miedo de escribir nombres largos. *alumnosAprobados* es mejor que *alsAp*.
-   Ser **claro y simple**.
    -   Por ejemplo, se pueden usar abreviaciones claras (como *cant* en vez de *cantidad*).
-   Respetar las **convenciones** es buena idea.
    -   Por ejemplo, el código escrito en lenguaje Python separa las palabras dentro de un nombre así: *esto\_es\_una\_variable*, mientras que en Smalltalk la convención es así: *estoEsUnaVariable*.
-   Dar idea (no muy específica) del **tipo** ayuda también a la expresividad.
    -   Por ejemplo, en el array del ejemplo de arriba hay una variable que se llama *numeros*. Esto da a entender rápidamente que son muchos, y que son de algún tipo numérico, exactamente cuál no es importante para entender qué hace. Ejemplos de malos nombres son *numero*, *ints* ó *arrayNumeros*. Tampoco es buena idea llamar a la función *realPromedio*, porque ensucia la legibilidad. Se sobreentiende que un promedio es de un tipo real ó flotante.

El tiempo que uno gasta en encontrar/**pensar un buen nombre**, es **tiempo bien aprovechado**.

#### Declaratividad

-   Ver [Declaratividad vs. Expresividad](declaratividad-vs--expresividad.html)

#### Identación

Identar el código (separar con espacios) es una buena manera de hacer que el código sea legible. Nuevamente, no debe abusarse de ésto, ni usarlo poco. Y respetar las convenciones del lenguaje es buena idea. En el ejemplo de arriba se ve claramente cómo la identación ayuda a leer mejor el programa, y de paso se respeta la identación sugerida de Pascal.
