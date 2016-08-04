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

Supongamos que queremos que un tanque que dispara misiles térmicos le dispare a otro tanque. El daño que hace un misil térmico es 10 veces la cantidad de ocupantes del tanque al que es disparado. El tanque enemigo tiene una coraza que va decrementando a medida que recibe daño (el mismo es destruído cuando la coraza llega a 0) y debe ser manejado por 3 personas. Una solución bien delegada podría ser:

**`Smalltalk:`**
`tanqueDeMisiles >> dispararA: otroTanque `
`   otroTanque recibirDaño: (misil cuantoDañoPara: enemigo).`
`tanqueEnemigo >> recibirDaño: cant`
`  coraza := (coraza - cant) max: 0`
`tanqueEnemigo >> cantidadOcupantes `
`  ^ 3`
`misilTermico >> cuantoDanioPara: unTanque`
`  ^ unTanque cantidadOcupantes * 10`

**`Wollok:`**
`object tanqueDeMisiles {`
`  var misil = misilTermico`
`  method dispararA(otroTanque){`
`    otroTanque.recibirDanio(misil.cuantoDanioPara(otroTanque))`
`  }`
`}`
`object tanqueEnemigo {`
`  var coraza = 100`
`  method recibirDanio(cant) {`
`    coraza = (coraza - cant).max(0)`
`  }`
` `
`  method cantidadOcupantes(){`
`    return 3`
`  }`
`}`
`object misilTermico {`
`  method cuantoDanioPara(unTanque){`
`    return unTanque.cantidadOcupantes()*10`
`  }`
`}`

Es particularmente importante que la forma de recibir daño esté delegada en el tanque enemigo, ya que no hay ningún motivo para que el tanque de misiles sepa de la existencia de una coraza del enemigo (mejor en términos de [encapsulamiento](encapsulamiento.html)).
