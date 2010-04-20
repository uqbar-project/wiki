A la hora de querer resolver un problema parados en el paradigma de objetos, hay ciertas cuestiones básicas que debemos tener en cuenta para triunfar y ser felices :). De todas aquellas, vamos a nombrar principalmente a las que se destacan en este paradigma en contraposición de los otros que conocemos (lógico, funcional y estructurado o procedural).

Objeto
------

Cuando programamos en objetos, debemos pensar en objetos. Solo tenemos objetos a los que mandarles mensajes para que nos resuelvan nuestro problema. A su vez, a veces es conveniente tener muchos objetos, para poder partir el problema en varias partes más pequeñas y que sea sencillo resolver cada parte por separado. Cada un de esos otros objetos van a tener una responsabilidad.

Responsabilidad
---------------

Es aquello que hace un objeto. Aquello que nosotros queremos que haga. Si tenemos un solo objeto que hace demasiadas o todas las tareas de nuestro programa (a.k.a. [God Object](god-object.html)), probablemente sea complicado que nuestro programa cambie y resuelva nuevos problemas. Va a ser complicado saber si funciona correctamente (testearlo). Va a ser complicado arreglar los errores que tenga!! (si es que los tiene). Si tenemos varios objetos con menos responsabilidades cada uno (mas chiquitos), va a ser más sencillo intercambiarlos usando [polimorfismo](polimorfismo.html), reutilizar partes de código en otras partes de nuestro programa, evitar [repetición de código](repeticion-de-codigo.html)... Por esas razones, es que queremos que nuestros objetos tengan bien repartidas las responsabilidades entre ellos.

Delegacion y Colaboración
-------------------------

Cuando tenemos bien repartidas las responsabilidades entre nuestros objetos, probablemente hayamos encontrado varios objetos. Ahora, cada uno resuelve su problema ¿Cómo los pegamos? Los objetos se conocen a través de referencias, y así pueden mandarse mensajes. Cuando un objeto resuelve parte de un problema y le pasa otra parte del problema a algun objeto que conozca, hablamos que ambos objetos estan colaborando. Así, cuando un objeto le encarga toooda la tarea a resolver a otro objeto, decimos que este delega la responsabilidad.
