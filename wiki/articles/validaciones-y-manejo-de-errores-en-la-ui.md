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

-   Una opción es usar booleanos (true = el valor es válido, false = es inválido), es simple pero no permite que el dominio indique el por qué el valor es inválido y nos obligará a colocar esa lógica en la UI.
-   Una extensión natural es usar un Enum, pero trae problemas similares: hay un alto \[\* [Acoplamiento](conceptos-basicos-del-diseno-acoplamiento.html) entre la UI y el dominio.
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

Para manejar las excepciones suele ser útil tener una excepción que sea base de todas las excepciones que contienen información para el usuario (en nuestro ejemplo la llamamos UserException). De esta forma la UI siempre que reciba una excepción de este tipo sabe que debe mostrar el mensaje de error al usuario y cualquier objeto de dominio que necesite informar un error lo puede hacer tirando una excepción de este tipo o alguna subclase.

Errores de sistema
------------------

¿Qué sucede si hay un error de programa?

-   Del lado del dominio, no hay mucho por hacer: o bien "envolvemos" la excepción de bajo nivel en una de mayor nivel, o bien dejamos que la excepción salte... después de todo: ¿qué podemos hacer con una NullPointerException? ¿o con un error en la construcción del programa? Lo mejor, es que el programa explote, pero de una forma elegante.
-   Entonces del lado de la UI, deberíamos atrapar esa excepción que se genere de manera de evitar que el Stack Trace se propague hasta el usuario.

Otras variantes
---------------

Un agregado que se podría hacer es evitar poner los mensajes de error en el código de dominio y poner códigos, que se traduzcan mediante un "bundle". Más adelante veremos eso.

Otros links relacionados
------------------------

[Algo3 Temario](algo3-temario.html)
