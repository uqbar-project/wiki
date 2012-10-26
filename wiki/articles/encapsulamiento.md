¿Qué es el encapsulamiento?
---------------------------

El encapsulamiento se refiere al ocultamiento de los atributos de un objeto para poder separar mejor las responsabilidades de cada objeto y evitar efectos inesperados como resultado de la modificación del valor de las variables por entidades externas.

El uso de setters y getters (mensajes para modificar y conocer el valor de un atributo respectivamente), también conocidos como accessors, es importante para que el objeto que tiene esos atributos pueda controlar el uso de los mismos y para que los que usan al objeto que los tiene no sufran un impacto muy grande si la implementación del mismo cambia.

Ejemplo
-------

Supongamos que representamos a los lugares a los que puede volar nuestra amiga Pepita la golondrina como objetos que conocen su kilometraje en una ruta y nos saben decir a qué distancia se encuentran de otro lugar. Si su estado interno se modificara de modo que su ubicación se represente por una coordenada (x e y), sólo los lugares deberían verse afectados por este cambio ya que a Pepita sólo le interesa conocer la distancia entre dos lugares.

A medida que el sistema crece esta característica toma más importancia ya que no es fácil determinar todos los lugares en los cuales algo se está usando y qué impacto tiene ese uso.
