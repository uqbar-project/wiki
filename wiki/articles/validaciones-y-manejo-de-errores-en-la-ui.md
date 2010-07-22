Introducción
------------

Una cuestión en la que hay que pensar es dónde poner las validaciones. Tenemos validaciones en la interfaz de usuario (o en el binding) y en el dominio. Recalcamos la importancia de poner en el dominio las validaciones de negocio, en los demás lugares deberían ir únicamente las validaciones propias de la interfaz de usuario, por ejemplo las conversiones de datos que requiere la interfaz.

Algunos ejemplos:

-   Convertir una fecha de String a Date es una cuestión de la interfaz, así que validar que el formato sea válido no le corresponde al dominio, el dominio recibe un Date y no sabe nada de los Strings.
-   Si la fecha de nacimiento de un empleado debe ser anterior a la fecha de hoy es una regla de negocio y sería bueno poder poner esa validación como código dentro del dominio.
-   Que la fecha es obligatoria también es una regla de negocio.

Interacción entre dominio y vista para manejar validaciones de negocio
----------------------------------------------------------------------

Una vez que tenemos validaciones que elegimos colocar en el dominio debemos pensar en qué forma el dominio informa los problemas a la interfaz de usuario, siendo que debe hacerlo en términos del dominio.

-   Una opción es usar booleanos (true = el valor es válido, false = es inválido), es simple pero no permite que el dominio indique por qué el valor es inválido y nos obligará a colocar esa lógica en la UI.
-   Una extensión natural es usar un Enum, pero trae problemas similares: hay un alto [acoplamiento](conceptos-basicos-del-diseno-acoplamiento.html) entre la UI y el dominio.
-   Otra opción es usar códigos de retorno del lado del dominio: -2 significa que la fecha de nacimiento es mayor a la fecha de hoy, -3 es que la fecha de nacimiento es anterior a 1900, etc. Sigue habiendo un alto acoplamiento entre los errores que el negocio dispara y el mensaje que la UI debe mostrar.
-   Lo que más nos gusta es marcar los errores con excepciones. Eso permite tener distintos tipos de error y también asociar un mensaje para el usuario:

<code>

`   >>Empleado`
`   public void validar() {`
`      ...`
`      if (!new Date().before(this.fechaNacimiento)) {`
`          throw new UserException("La fecha de nacimiento tiene que ser anterior al día de hoy");`
`      }`
`      ...`
`   }`

</code>

Para manejar las excepciones suele ser útil tener una excepción que sea base de todas las excepciones que contienen información para el usuario (en nuestro ejemplo la llamamos UserException). De esta forma siempre que la UI reciba una excepción de este tipo sabe que debe mostrar el mensaje de error al usuario y cualquier objeto de dominio que necesite informar un error lo puede hacer tirando una excepción de este tipo o alguna subclase. De esa manera la UI no conoce nada de la lógica que maneja el dominio y mantenemos bajo el acoplamiento entre ambos componentes.

Errores de sistema
------------------

¿Qué sucede si hay un error de programa?

-   Del lado del dominio, no hay mucho por hacer: o bien "envolvemos" la excepción de bajo nivel en una de mayor nivel, o bien dejamos que la excepción salte... después de todo: ¿qué podemos hacer con una NullPointerException? ¿o con un error en la codificación del programa? Lo mejor es que el programa explote, pero de una forma elegante.
-   Entonces del lado de la UI, deberíamos atrapar esa excepción que se genere de manera de evitar que el Stack Trace se propague hasta el usuario.

Pero ¿dónde? Una buena regla es utilizar try/catch en los lugares en los que, de otra manera, la aplicación mostraría el stack trace al usuario. Nosotros no queremos que eso suceda, pero es bueno recalcar que es preferible que el usuario llame por una pantalla rota a que el error quede escondido, como en estos típicos casos: <code>

`   >>Código de pantalla`
`   public void buscarSocios() {`
`      try {`
`          ... hacemos la búsqueda ...`
`      } catch (Exception e) {`
`          e.printStackTrace();`
`      }`
`   }`

</code> El usuario no va a reportar que la búsqueda tira error, sino que "ciertas" búsquedas no funcionan. A veces trae datos y a veces no. De esa manera la aplicación pierde robustez.

### ¿Qué va en el catch del lado de la pantalla?

Por un lado, hay que guardar el error en un archivo de log dentro del servidor donde corre la aplicación en algún formato amigable que nos permita encontrarlo rápidamente. Para ello existen algunas librerías como Log4J y loggers alternativos que ayudan a

-   activar y desactivar distintos niveles de criticidad de los mensajes (debug/info/warn/error)
-   enviar los mensajes de error a distintos archivos de log en base a la aplicación/package de la clase donde se originó el error

Por otra parte, necesitamos informar al usuario que cuando estábamos buscando los socios, cuando queríamos agregar el socio, cuando estábamos inicializando la pantalla, etc. ocurrió un error y la aplicación no va a poder responder correctamente el pedido. Y eso depende mucho de la tecnología en la que nos encontramos.

-   Si la tecnología de UI se programa en Java tengo posibilidad de enviar un message box modal al usuario
-   Si la tecnología de UI corre sobre un browser, a veces tengo que redirigir el error a una página especial y a veces puedo mostrar en un área de mensajes el error dentro de la misma página

Un ejemplo genérico de manejo de errores en UI
----------------------------------------------

<code>

`   >>Código de pantalla`
`   public void agregarSocio() {`
`      try {`
`          Socio socio = ... recolectamos la información de la UI ...`
`          socio.validar();`
`          ... pedimos que generen el socio, esto depende de nuestra estrategia ...`
`      } catch (UserException e) {`
`          this.messageWarning(e.getMessage());`
`      } catch (Exception e) {`
`          this.logger.fatal("AppSocios", e.getMessage());`
`          this.messageError("Ocurrió un error al agregar el socio. Por favor consulte al administrador del sistema");`
`      }`
`   }`

</code> Los métodos messageWarning y messageError, como dijimos antes, dependen de la tecnología a implementar, en nuestro caso asumimos que está en una superclase de Form de la cual hereda la pantalla de Alta de un Socio.

El logger es un objeto que sabe guardar el stack trace en un formato configurable y en un archivo x que depende del primer parámetro que le paso (por ejemplo, el AppSocios guarda el archivo Videoclub.log en el directorio de log default).

Otras variantes
---------------

Un agregado que se podría hacer es evitar poner los mensajes de error en el código de dominio y poner códigos, que se traduzcan mediante un "bundle". Más adelante veremos eso.

Otros links relacionados
------------------------

[Algo3 Temario](algo3-temario.html)
