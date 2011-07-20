Problemas con el entorno
------------------------

Estoy tratando de pasar algunas clases del tp de objetos y no puedo crear clases.

\#\* Fijate que el nombre de la clase este en mayúsculas.

Interpretación de errores
-------------------------

Evalué mi código, me tiró un error y no sé lo que significa

\#\* Don't panic

\#\* Leé el título de la ventana de error

\#\* Si con eso no alcanza para entender, lee la cadena de mensajes que se fueron enviando (stacktrace). Cada línea del stacktrace dice:

\#\*\*la clase del objeto receptor, si el receptor era una clase dice además class, por ejemplo Golondrina class

\#\*\*entre paréntesis la clase en donde está definido el método que se ejecutó

\#\*\*el mensaje enviado a ese objeto

\#\*Si todavía no sabés qué fue, apretá Debug que te aporta mucha más información para entender qué pasó

**Mensajes más comunes:** *MessageNotUnderstood* Problema: Se mandó un mensaje el receptor no entiende. Posibles soluciones:

-   Revisar que el objeto receptor fuera correcto (si es nil probablemente haya algo sin inicializar)
-   Revisar que no haya error de tipeo (el mensaje coincide con el nombre del método definido)
-   Revisar que el método esté definido en el lugar correcto (del lado de las instancias si es un método de instancia y del lado de la clase si el receptor es una clase)

*NonBooleanReceiver: proceed for truth* Problema: Se le mandó a un objeto que no es booleano un mensaje de booleanos como ifTrue:ifFalse: Solución: Mirar el stacktrace, en algún momento va a aparecer: ClaseDelObjetoReceptor(Object)&gt;&gt;mustBeBoolean más abajo en el stacktrace debería aparecer un mensaje que definieron ustedes o DoIt si se envió desde el workspace. Darle doble click a ese mensaje para abrir el debugger y ver qué pasó.

*Error: This block accepts <n> arguments, but was called with <m> arguments* Problema: se evaluó un bloque con una cantidad de argumentos distinta de la esperada. Solución: Buscar en el stacktrace el método que definieron (o DoIt si fue directo desde el workspace) que evaluó el bloque con una cantidad incorrecta de parámetros.
