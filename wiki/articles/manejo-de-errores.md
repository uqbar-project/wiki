Consistencia
------------

Al producirse una excepción, es importante garantizar la consistencia del modelo (el estado de mi programa) antes de continuar. Esto significa que la operación debe realizarse o no en su totalidad, no puede quedar en un estado intermedio.

Algunas estrategias para resolver eso pueden ser:

-   Lo más simple parece ser rollbackear, es decir: si en una secuencia de operaciones falla la enésima, hay que volver atrás el efecto de las n-1 anteriores. Esto es lo más simple de pensar, pero no siempre es lo más simple de programar.
    En un caso donde diferentes cosas pueden fallar, puede ser complejo determinar cuál es la que falló para luego rollbackear las anteriores. Esa indefinición puede producir una cadena interminable de ifs dentro del catch, por lo tanto es algo que será preferible evitar.
-   Si en un conjunto de operaciones que deben realizarse atómicamente hay una sola que puede fallar, ponerla en primer lugar. Eso nos evita los rollbacks.
-   Una variante de lo anterior es poner primero las que son más fáciles (o más baratas en algún sentido) de rollbackear.
-   Como última alternativa, en caso de ser imposible rollbackear, suele ser preferible informar el problema y cerrar la aplicación que dejarla corriendo en un estado potencialmente inconsistente. En este caso se estaría apelando a una solución manual.

¿Dónde poner el return?
-----------------------

Esta pregunta esta en el límite de lo que es una buena práctica y el gusto personal. Lo que no hay que perder de vista es que ayude la legibilidad del código respetando las premisas del buen manejo de excepciones (¡¡que explote cuando tiene que explotar!!)

Lo ideal es que cuando haya que usar un try/catch, quede de la siguiente forma

Si es un catch que agrega informacion a la exception y la lanza para arriba

<code font="">

`public Object method() {`
`   try{`
`     //codigo del metodo`
`     return ... //return por las cosas normales del metodo`
`   }`
`   catch(Exception e) {`
`     //genero una nueva exception con la info adicional`
`     throw ... // lanzo la nueva exception`
`   }`
`}`

</code>

si es un método que tiene que realizar el manejo de la excepción, entonces puede ocurrir que el catch y el try tengan su propio return: <code>

`public Object method() {`
`   try{`
`     //codigo del metodo`
`     return ... //return por las cosas normales del metodo`
`   }`
`   catch(Exception e) {`
`     //manejo de la exception`
`     return ..//return por el caso de error`
`   }`
`}`

</code>

Si el error que ocurre tiene un contexto que no abarca todo el método, por ejemplo <code>

`public Object method() {`
`  ...`
`  //Codigo`
`  ...`
`   try{`
`     //codigo del metodo`
`   }`
`   catch(Exception e) {`
`     //manejo de la exception o lanzamiento de otra`
`   }`
`   ...`
`   // mas codigo Codigo`
`   ...`
`   return ...`
`}`

</code> entonces lo que conviene es extraer ese bloque try/catch en un método privado o protected (seguramente si tengo este caso es que me falla la [cohesión](conceptos-basicos-del-diseno-cohesion.html) dentro del método y puedo desmenuzar el metodo en métodos más chicos y cohesivos)

<code>

`public Object method() {`
`  ...`
`  //Codigo`
`  ...`
`  this.methodPrivate();`
`  ...`
`  // mas codigo Codigo`
`  ...`
`  return ...`
`}`
`private void methodPrivate() {`
`   try {`
`     //codigo del metodo`
`     return del caso normal si hubiera`
`   }`
`   catch(Exception e) {`
`     //manejo de la exception o lanzamiento de otra`
`   }`
`}`

</code>
