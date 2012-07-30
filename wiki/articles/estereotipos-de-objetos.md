`Inmutable vs mutable`

Decimos que un objeto es inmutable cuando su estado es insignificante, o se mantiene constante a lo largo de su ciclo de vida, o, en una visión más relajada, luego de su momento de ínstanciación.

Identidad

En el paradigma de objetos, estos componentes agrupan lo que en otros paradigmas en general está claramente separado: datos (estado) y comportamiento. Además, los entornos de objetos los manejan mediante referencias (C++ es una excepción notable), lo que nos lleva a otra propiedad fundamental: la identidad.

En base a estos tres aspectos del objeto, podemos ensayar la siguiente caracterización de los objetos:

Si bien, en general lo que nos importa de un objeto es su capacidad de responder mensajes...

Algunas combinaciones nos llevan a: Identidad importante, estado y comportamiento inexistentes: símbolos, candados. Identidad y estado importante: entidades Comportamiento importante, estado e identidad insignificantes: objeto función, stratey stateless,

`Comportamiento ...`

Semántica de referencia vs semántica de valor

-   Su identidad no es importante
-   (Consecuencia de lo anterior) Su estado, si importante, es constante.

