Definición
----------

La inversibilidad es una característica qué podés encontrar únicamente en el paradigma lógico (por lo menos hasta donde sé). Que un predicado sea inversible significa que los argumentos del mismo no tienen una "dirección". Es decir, no necesitás "pasar" el parámetro resuelto (unificado con un valor), sino que podés pasarlo sin unificar (como si fuera una incógnita).

Imaginate un predicado `esNatural/1` que te dice si un número es natural:

    esNatural(1)
    Yes

    esNatural(-2)
    No

Cuando un predicado es inversible, también podés hacer otro tipo de consultas, con el parámetro sin unificar:

    esNatural(N)
    N=1;
    N=2;
    N=3;
    etc...

Esta característica es exclusiva del paradigma lógico, ya que está basado en el concepto de relación matemática (y no en el de función).

== ¿Cómo hacés que un predicado sea inversible? Con lo que tenés que tener cuidado es con las submetas de un predicado que requieren variables unificadas. Por ejemplo: cuando una de las componentes de un predicado es un not/1 , necesitás que el predicado que le mandás tenga sus parámetros unificados. Cuando usás una evaluación matemática (ej: X is N \* 2), necesitás que lo de la derecha (la N) esté unificado.

¿Cuál sería el uso?
-------------------

Inherentemente es más potente que una función, ya que te permite encontrar todos los valores para los cuales una relación se cumple (y esto tiene que ver también con la idea de "múltiples resultados"...).

La ventaja inmediata es que te permite más formas de usarlo, lo típico: puedo preguntar si un alumno aprobó, o todos los alumnos que aprobaron, etc.

Luego podría aparecer otra ventaja y es que si todos mis predicados son inversibles, eso de verdad me permite usarlos sin pensar nada nada nada en la secuencia y me da más lugar a sólo declarar el conocimiento. Y eso le da una potencia más grande a mi lenguaje/paradigma/entorno de programación.

(Igual no debe entenderse de lo último que todos los predicados que hago deben ser inversibles. )
