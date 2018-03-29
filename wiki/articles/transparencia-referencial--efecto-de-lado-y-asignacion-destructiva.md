---
layout: article
title: Transparencia referencial  efecto de lado y asignacion destructiva
---

Definiciones
------------

**Operación:** aplicar una función, evaluar un predicado, enviar un mensaje, etc.

**Transparencia Referencial**

Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto.

Una definición alternativa dice: Hay transparencia referencial cuando al realizar una operación con los mismos valores siempre da el mismo resultado. Si bien esta parece más fácil de entender, no es tan precisa como la primera; puede ser útil para dar los primeros pasos, pero para el final hay que terminar de entender la otra.

También se puede pensar en las propiedades necesarias para tener Transparencia Referencial.

Decimos que una operación tiene transparencia referencial si es:
- Independiente: No dependen del estado de nada que este fuera de sí misma
- Sin estado/Stateless: No tiene un estado que se mantenga de llamada en llamada
- Determinística: Siempre devuelven el mismo valor dados los mismos argumentos
- No produce efecto colateral

**Efecto de Lado/Colateral (Side Effect)**

Hay efecto cuando un cambio de estado sobrevive a la realización de una operación. Por ejemplo, una operación puede modificar una variable global, modificar uno de sus argumentos, escribir datos a la pantalla o a un archivo, o hacer uso de otras operaciones que tienen efecto de lado.

Otra definición válida es: Si le sacás una foto al sistema (llamémosla F1), después realizas la operación de tu interés, y le volvés a sacar una foto al sistema (F2). Si F1 y F2 son distintas =&gt; la operación que hiciste tiene efecto de lado.

**Asignación Destructiva**

Asignar destructivamente es reemplazar el valor de una variable por otro valor.

La [unificación](unificacion-y-pattern-matching.html) no se considera asignación (al momento de ligar no había ningún valor anterior, ¿sería más bien una inicialización?). Unificar es encontrar una sustitución capaz de igualar 2 términos. Cuando se efectiviza está sustitución hablamos de ligado de variables (tal valor se ligó a tal variable).

Ejemplos
--------

Cuando hablamos de que "algo" tiene transparencia referencial, efecto colateral o asignación destructiva, ese "algo" es la realización de una operación, de un lenguaje en particular o de un paradigma.

Estos tres conceptos suelen ir de la mano y si bien pueden darse relaciones entre ellas es saludable poder detectar la aparición de cada uno de ellos individualmente. Una relación que surge de la definición de transparencia referencial es que para ésta se dé, no puede haber efecto colateral, ya que si el estado del sistema se modifica, o se escribe en un archivo por ejemplo, no es lo mismo ejecutar esa operación que reemplazar por el resultado.

A continuación mostramos algunos ejemplos en el [paradigma orientado a objetos](paradigma-de-objetos.html), ya que permite la aparición de todas estas características, para dejar más en claro de qué manera podemos identificarlas.

### Ejemplo 1: consulta no determinística

El siguiente código crea una fecha, configurada para representar el día de hoy: `new Date()`

-   Transparencia Referencial: NO (Con cualquiera de las 2 definiciones de transparencia referencial)
-   Efecto: NO
-   Asignación Destructiva: NO

Evaluarlo con los mismos parámetros (o sea ninguno) en días distintos va a dar resultados distintos. Reemplazar la operación por el resultado una vez que cambia el día se rompe todo. Asignación destructiva y efecto no hay, o al menos no es relevante (se está creando un nuevo objeto en el sistema, pero en general no lo vamos a considerar para nuestro análisis, y tampoco tiene que ver con el hecho de que un día responda una cosa y otro día otra).

Ejemplos como este hacen que transparencia referencial y efecto colateral no sean conceptos opuestos, ya que en este caso se debe a que la operación depende de algo externo (la fecha de la computadora).

### Ejemplo 2: método con efecto

Dada la siguiente implementación del objeto pepita:

```
object pepita {
  var energia = 100
  method vola(metros) {
   energia = energia - (metros + 4)
  }
}
```

Analicemos el mensaje: `pepita.vola(20)`

-   Efecto colateral: SI, porque la energía de pepita antes era 100 y luego es 76.
-   Transparencia Referencial: NO, se está produciendo un efecto al disminuirse la energía de pepita. En este caso el método no retorna un valor, con lo cual no tendría sentido intentar reemplazar ese envío de mensajes por su resultado.
-   Asignación destructiva: SI, al hacer energia = ... estamos cambiando a qué objeto referencia por esa variable.

### Ejemplo 3: método de consulta determinística

```
object factorial {
  method para(numero){
    var resultado = 1
    if(numero > 0) 
      resultado = self.para(numero - 1) * numero
    return resultado
  }
}
```

Analicemos el mensaje: `factorial.para(20)`

-   Transparencia Referencial: SI, el resultado sólo depende de sus argumentos, no importa en qué contexto, siempre dará el mismo resultado para el número 20.
-   Asignaciones Destructivas: SI, podemos ver que la variable local resultado primero toma el valor 1, pero luego para números mayores a 0 se modifica por el valor que corresponda.
-   Efecto colateral: NO, a pesar de que hay una asignación dentro del método, al ser sólo una variable local no se produce ningún efecto que perdure a la ejecución de ese mensaje. Esa asignación podría analizarse como efecto colateral dentro del método. Probablemente en un método tan pequeño como este no tenga importancia ese tipo de análisis, pero en el caso de algoritmos más complejos podría cobrar valor (y asumiendo que no sea posible partir un algoritmo complejo en operaciones más pequeñas que simplifiquen justamente el análisis, pero eso ya es otra cuestión).

¿Por qué nos interesa pensar en estos conceptos?
------------------------------------------------

Estos son algunos ejemplos concretos sobre cómo la existencia o no de efecto, asignación destructiva y transparencia referencial afectan a la hora de programar.

**Separar la lógica que hace cosas de la que consulta:** Muy seguido vemos métodos (o procedimientos, dependiendo del paradigma) que tienen efecto y a su vez retornan algún valor relacionado con el mismo, estas prácticas pueden llevar a confusiones que producen un funcionamiento erróneo del sistema, sobre todo cuando el nombre del método elegido no denota que existe un efecto asociado a su ejecución. Es una buena práctica tener separada la lógica que realiza modificaciones sobre el sistema de los que sólo pretenden obtener el resultado de una consulta, que nuestros métodos tengan un único objetivo, lo cual simplifica su uso y la elección de un nombre suficientemente representativo.

**Respetar los contratos blandos:** Un contrato blando es algo que cierta pieza de código requiere que cumpla el usuario para que la misma funcione de la forma esperada, pero esos requisitos no son validados de ninguna forma. Un ejemplo típico de esto está relacionado con los [mensajes de colecciones](mensajes-de-colecciones.html) que esperan recibir un bloque de código que sea sólo de consulta, o sea que no produzca ningún efecto.

**Optimizaciones:** Tener asegurada la transparencia referencial permite hacer optimizaciones como las que tiene el motor de Haskell que afectan globalmente a los programas construidos con el mismo. La [evaluación perezosa o lazy](estrategias-de-evaluacion-lazy-evaluation.html) es posible gracias a esta característica. También lo podemos ver en Prolog que para buscar soluciones utiliza el mecanismo de [Backtracking](backtracking.html) de modo que se puedan encontrar múltiples respuestas a una consulta, así como descartar los caminos por los cuales no sea posible hayar alguna, de una forma eficiente.

Otro ejemplo viene de la mano del procesamiento en paralelo. Si tenemos un conjunto con millones de elementos y queremos filtrarlo por un criterio, ¿no puedo dividir ese conjunto en varios más pequeños, filtrarlos por separado en distintos procesadores y juntar sus resultados? Si yo aseguro que evaluar el criterio de filtrado sobre cada elemento no va a provocar ningún efecto que pueda alterar mi resultado final, esta optimización podría permitir aprovechar mucho mejor el hardware disponible. Si te interesa el tema acá hay algo para leer al respecto: [Scala - Parallel Collection Framework](http://infoscience.epfl.ch/record/150220/files/pc.pdf)

**[Testing](testing.html):** El testeo unitario se basa en la premisa de que cada test sea independiente del otro y eso se logra controlando que el estado del sistema antes y después de correr cada test sea el mismo, por ese motivo es importante mantener el efecto controlado y poder revertir aquellos cambios que sobrevivan a la ejecución de cada test particular. También la transparencia referencial es importante para el testeo unitario ya que testear el resultado de una operación que depende de algo no determinístico no es viable y hace falta usar estrategias de testeo más avanzadas ([Mock Objects](http://es.wikipedia.org/wiki/Objeto_simulado)) para evitar este tipo de dependencia.

Preguntas frecuentes
--------------------

Leí una definición de Transparencia Referencial: “Hay transparencia referencial si al reemplazar una operación por su resultado se logra el mismo efecto”

Con este criterio, aquí sí habría transparencia referencial:

```
int a=1;
int c;
c=a++;
```

Ya que es lo mismo que hacer esto:

```
int a=1;
int c;
c=1;
```

Ya que el efecto en la variable c es el mismo: va a valer 1.

Pero para mí no es así, ya que no va a haber transparencia referencial, porque si bien se logra el mismo efecto con respecto a c, el sistema cambia (la variable a se incrementa en una unidad).

¿Es correcto afirmar entonces que si no hay Efecto de Lado entonces tengo garantizada la Transparencia referencial y viceversa?

> En el ejemplo dado no hay transparencia referencial, es correcta la interpretación. La expresión a++ tiene el efecto colateral de modificar el valor de a, por lo tanto no puede tener transparencia referencial. Sin embargo, no es correcto que si no hay efecto entonces está garantizada la transparencia referencial (como se pone en evidencia en el ejemplo de la consulta no determinística).
