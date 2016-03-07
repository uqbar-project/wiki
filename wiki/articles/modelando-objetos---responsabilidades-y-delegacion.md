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

Delegación y polimorfismo
-------------------------

La situación donde más natural es delegar es al querer tratar polimórficamente a varios objetos, ya que les mandamos un mensaje común y delegamos en ellos lo que le corresponde a cada uno dependiendo de qué tipo sea. Sin embargo, se puede hablar de delegación, sin necesidad de que haya polimorfismo.

### Ejemplo

**`Smalltalk:`**
`Tanque >> dispararA: otroTanque`
`   | unMisil |`
`   unMisil := misiles anyOne.`
`   misiles remove: unMisil.`
`   danioTotal := unMisil cuantoDañoPara: otroTanque.`
`   otroTanque coraza > danioTotal `
`     ifTrue: [ otroTanque coraza: otroTanque coraza - danioTotal ]`
`     ifFalse: [ otroTanque coraza: 0 ].`

**`Wollok:`**
`class Tanque {`
`  method dispararA(otroTanque) {`
`    var unMisil = misiles.anyOne() `
`    misiles.remove(unMisil) `
`    var danioTotal = unMisil.cuantoDanioPara(otroTanque) `
`    if (otroTanque.coraza() > danioTotal) {`
`      otroTanque.coraza(otroTanque.coraza() - danioTotal )`
`    } else {`
`      otroTanque.coraza(0)`
`    }`
`  }`
`}`

Fíjense que en el ejemplo anterior, si asumimos que hay muchos tipos de misiles, desde el tanque los tratamos polimórficamente envíandoles el mismo mensaje para obtener el daño que recibirá el otro tanque, pero no sé si delegamos lo suficiente.

Veamos esta otra solución que delega mucho más que la anterior:

**`Smaltallk:`**
`Tanque >> dispararA: otroTanque`
`   self descargarMisil dañarA: otroTanque`
`Tanque >> descargarMisil`
`  "El remove: devuelve el parámetro"`
`  ^misiles remove: misiles anyOne`
`Tanque >> recibirDaño: cant`
`  self coraza: ((self coraza - cant) max: 0)`
`Misil >> dañarA: otroTanque`
`  "Asumimos que Misil es la superclase de todos las otras clases de misiles"`
`  otroTanque recibirDaño: (self cuantoDañoPara: otroTanque)`

**`Wollok:`**
`class Tanque {`
`  method dispararA(otroTanque) {`
`    self.descargarMisil().daniarA(otroTanque)`
`  }`
`  method descargarMisil() {`
`    var unMisil = misiles.anyOne();`
`    misiles.remove(unMisil)`
`    return unMisil`
`  }`
`  method recibirDanio(cant) {`
`    var corazaActual = (self.coraza() - cant).max(0)`
`    self.coraza(corazaActual)`
`  }`
`}  `
`class Misil {`
`  method daniarA(otroTanque) {`
`  //Asumimos que Misil es la superclase de todos las otras clases de misiles`
`  otroTanque.recibirDanio(self.cuantoDanioPara(otroTanque))`
`  }`
`}`

También se puede hablar de extensibilidad, si el día de mañana se agrega un nuevo misil (el misil Gandhi) que en vez de sacar coraza le hace un cariñito al otro tanque Con la segunda solución eso se agrega bastante fácil

**`Smalltalk:`**
`MisilGandhi >> dañarA: otroTanque`
`  otroTanque recibirCariñito`

**`Wollok:`**
`class MisilGandhi {`
`  method daniarA(otroTanque) {`
`    otroTanque.recibirCarinito()`
`  }`
`}`

La segunda solución es más extensible. Lo malo de este agregado es la [expresividad](declaratividad-vs--expresividad.html), porque "dañarA" me da la sensación de que siempre le saca coraza ... tal vez deberíamos buscar un mejor nombre que no implique que exista un daño sobre el otro tanque.
