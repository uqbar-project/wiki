Es importante reconocer el ciclo de vida de un objeto, ya que el estado que puede guardar un objeto depende de eso.

Tipos de ciclo de vida puede haber mucho, a modo de ejemplo podemos mencionar tres muy comunes:

-   Objetos persistentes o entidades: Son los que representan los datos que guardamos de nuestro negocio, las entidades permanentes, por ejemplo en un video club me guardo los socios, las películas. Típicamente el ciclo de vida es darlos de alta, modificarlos, eventualmente se pueden dar de baja. A veces la baja es lógica.
-   Objetos transientes o procesos: Son los que guardan información sobre un proceso que se está llevando a cabo. El proceso puede ser tan corto como el procesamiento de un evento de teclado o mouse o un proceso de negocio que dura meses. Sin embargo los objetos persistentes no deberían guardar información de un proceso puntual, ya que esto es fuente de errores. En general los objetos con ciclos de vida más largos no pueden tener referencias a objetos de ciclo de vida más corto puntuales. Una vez terminados, estos objetos a veces se guardan como histórico. Un objeto persistente sí puede tener referencia a los procesos históricos o bien a los procesos en curso.
-   Objetos permanentes, singletons, servicios. Son objetos que duran toda la vida del sistema (con variantes). En general nacen con el sistema y no pueden ser agregados nuevos, son compartidos por todos los usuarios del sistema. Estos son los que tienen el ciclo de vida más largo y no pueden tener ningún tipo de estado conversacional.

En una aplicación web suelen aparecer objetos cuyo ciclo de vida está asociado a un request o a una session.

Esto es análogo a un análisis de cardinalidad o normalización, se podría extender por ese lado.
