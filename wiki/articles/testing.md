Probar una aplicación permite verificar que el sistema (o una parte de él) funciona de acuerdo a lo especificado. Los tests permiten bajar la incertidumbre y aumentar la confianza que tenemos sobre el software que desarrollamos.

Hay diferentes formas de probar una aplicación, por ejemplo si tiene una interfaz de usuario el testeo podría realizarse manualmente siguiendo los pasos necesarios (eligiendo opciones de algún menú, llenando formularios, etc) para verificar que el sistema responde de la forma esperada. Programáticamente estamos acostumbrados a probar el sistema usando la funcionalidad desarrollada desde el mismo entorno de desarrollo, por ejemplo escribiendo en un workspace de Smalltalk. Supongamos que tenemos este ejemplo:

`#Golondrina`
`>> puedeVolar`
`    ^ self energia > 20`
`>> vola: unosKilometros`
`    self puedeVolar ifFalse: [ NoPuedeVolarError signal ].`
`    self energia: self energia - (unosKilometros * 2 - 8)`

`pepita := Golondrina new.`
`pepita energia: 100.`
`pepita vola: 25. "Le resta 25*2+8 de energía"`
`pepita energia. "Debería retornar 42"`
`pepita puedeVolar. "Me debería decir que sí porque tiene más de 20 de energía"`
`pepita vola: 10. "Le resta 10*2+8 de energía"`
`pepita puedeVolar. "Me debería decir que no porque tiene menos de 20 de energía"`
`pepita vola: 1. "Debería explotar porque pepita no puede volar"`

Y al ejecutar ese workspace se puede verificar si el valor retornado por el mensaje energia luego de volar 20 kilómetros retorna el valor que esperamos que retorne, si pepita puede volar cuando tiene más de 20 de energía y que no puede cuando tiene menos, y si efectivamente ocurre un error si se intenta volar cuando pepita no puede volar.

Sin embargo existe otra forma de realizar pruebas de forma programática que permite automatizar todo el proceso de testeo, ya que verificar que el valor de la energía de pepita sea el esperado es algo que hay que hacer manualmente. En Smalltalk vamos a usar un framework llamado SUnit (existe también para otros lenguajes con otro nombre, búsquenlo, aprovéchenlo!) que lo que permite es codificar no sólo los pasos que describen la prueba, sino también la validación de los resultados esperados.

Automatizar las pruebas requiere un esfuerzo extra, pero es una gran inversión porque esas pruebas quedan disponibles para ser ejecutadas N veces en el futuro. A medida que el sistema crece, la cantidad de casos a probar para verificar que el sistema completo está bien construido aumenta muchísimo, y poder asegurar que toda la funcionalidad que programé ahora y que fue programada antes sigue funcionando es muy valioso.

Testing con SUnit
-----------------

Veamos cómo quedaría el ejemplo anterior, que estaba suelto en un workspace, usando SUnit. En primer lugar, ya no va a estar suelto sino que tiene que estar en un método de una clase de prueba, para indicar que es una clase de prueba y puede ser corrida como tal por la herramienta, la misma debe heredar de TestCase.

`TestCase subclass: #GolondrinaTest`
`   instanceVariableNames: ''`
`   classVariableNames: ''`
`   poolDictionaries: ''`
`   category: 'Pepita-Testing'`

Luego podemos agregar el método para verificar que cuando pepita vuela pierde energia en función de la cantidad de kilómetros volados. Lo que nos requiere el framework es que los nombres de los métodos que yo quiera que se corran como pruebas empiecen con la palabra test, lo que nos requieren las buenas prácticas y nuestra salud mental es que el resto del nombre describa lo mejor posible qué es lo que ese método pretende testear. También es importante poder separar la lógica en unidades atómicas e independientes, de modo que cada método de test tenga un único objetivo.

`#GolondrinaTest >> testCuandoPepitaVuelaPierdeEnergiaEnBaseALosKilometrosVolados`
`   |pepita|`
`   pepita := Golondrina new.`
`   pepita energia: 100.`
`   pepita vola: 25.`
`   `**`self` `assert:` `pepita` `energia` `equals:` `42.`**

El mensaje assert:equals: lo que hace es verificar que el resultado de una expresión sea igual a un objeto esperado.

Si al correr este caso de prueba pepita efectivamente queda con una energía de 42, el resultado del test será correcto (lo cual se muestra con algún indicador verde que dependerá de la herramienta, como ser una bolita al lado del método).

Si el resultado de pepita energia es otro valor, como por ejemplo 100 (porque nuestro método vola: existe pero no hace nada) el test falla, indicándose con otro color como ser amarillo, lo cual debe interpretarse como que el código se pudo ejecutar sin problemas pero el resultado obtenido fue distinto al esperado. Cuando esto sucede hay que corregir el problema que bien podría estar en el código de vola: como en el método de test porque no describe lo que realmente debería pasar, con lo cual lo que uno hace en estas situaciones por lo general es debuggear el método de test y analizar qué sucede a medida que se ejecuta el código para encontrar el problema.

Finalmente otra cosa que podría pasar es que el código no funcione, por ejemplo si pepita no entiende el mensaje vola: se genera un error y no se puede terminar la ejecución del test, indicándose con color rojo.

Continuemos con los casos de prueba que nos faltan. Si yo quiero testear cuándo puede volar pepita podríamos tener dos métodos diferentes, uno para verificar cuándo sí puede, y otro para verificar cuándo no.

`#GolondrinaTest >> testPepitaPuedeVolarSiTieneMasDe20DeEnergia`
`   |pepita|`
`   pepita := Golondrina new.`
`   pepita energia: 100.`
`   `**`self` `assert:` `pepita` `puedeVolar`**

El mensaje assert: recibe una expresión booleana, si esa expresión retorna true se considera correcto. Para hacer el otro caso en donde queremos validar que no pueda volar podríamos también usar assert: negando la condición (pepita puedeVolar not) o alternativamente usar el mensaje deny: que se verifica si la condición recibida es falsa.

`#GolondrinaTest >> testPepitaNoPuedeVolarSiTieneMenosDe20DeEnergia`
`   |pepita|`
`   pepita := Golondrina new.`
`   pepita energia: 10.`
`   `**`self` `deny:` `pepita` `puedeVolar`**

Por último queríamos verificar que si pepita no puede volar y le pedimos que vuele tiene que tirar un error. Si tuviéramos este código:

`#GolondrinaTest >> testSiPepitaDebeVolarYNoPuedeTiraError`
`   |pepita|`
`   pepita := Golondrina new.`
`   pepita energia: 10.`
`   pepita vola: 1.`

Cuando corramos este test, si efectivamente tira error porque no puede volar, el resultado que nos va a mostrar la herramienta será ROJO. Pero si nosotros queríamos verificar que tire error, cómo hacemos para transformar ese ROJO en VERDE?

`#GolondrinaTest >> testSiPepitaDebeVolarYNoPuedeTiraError`
`   |pepita|`
`   pepita := Golondrina new.`
`   pepita energia: 10.`
`   `**`self` `should:` `[pepita` `vola:` `1]` `raise:` `NoPuedeVolarError`**

El mensaje should:raise: espera un bloque que podría tirar error al evaluarlo, y en el caso de que suceda y el error sea instancia de la clase NoPuedoVolarError (o de alguna subclase) el resultado será verde. Si el error no sucede, el resultado será amarillo para indicar que no sucedió lo que se esperaba. También existe la versión contraria shouldnt:raise: que verifica que el bloque no tire error.

Armando un escenario
--------------------

En todos los casos anteriores lo primero que hacemos es crear la golondrina y settearle una cantidad de energía que corresponda (una con 100 para que pueda volar, otra con 10 para que no pueda). Suponiendo que por defecto vamos a querer usar una golondrina que puede volar, con lo cual tener a pepita con 100 de energía inicial, podríamos definir que antes de correr cada método de prueba de la clase GolondrinaTest hay que crear a pepita e inicializarla de esa forma. Para eso el framework nos pide que definamos un método llamado setUp de modo que todo lo que quiera que suceda antes de cada test se haga ahí, y luego sólo uso lo que se haya generado en los métodos de test.

`#GolondrinaTest >> setUp`
`  pepita := Golondrina new. "Nótese que pepita es una variable de instancia de la clase GolondrinaTest"`
`  pepita energia: 100.`

Luego podemos sólo hacer lo siguiente asumiento que pepita existe y tiene 100 de energía.

`#GolondrinaTest >> testPepitaPuedeVolarSiTieneMasDe20DeEnergia`
`  self assert: pepita puedeVolar`

Pueden ver otros ejemplos usando tanto SUnit nativo como con LOOP en este apunte: [Apunte de Testing en Smalltalk (con y sin LOOP)](https://4924d24e-a-62cb3a1a-s-sites.googlegroups.com/site/paradigmasdeprogramacion/Cursos/sabados-a-la-manana-anual-2012/PruebasUnitariasEnSmalltalk.pdf?attachauth=ANoY7cr7qvrGe95WAgr7-ZrpqizA8HqB-2kPZ7kAk11zguNxsyp95NGD6aY9ol2dz9yT6hHVwGxE0sn4I4Ifo_Vm5K2BNYMo6WiFxa8fIYAkBhmrXUAKtsHBrjrQfcMVnaPzB-EP3dqcoFsbFC6tKTKondZAWfSDMr84oJdxnNSUJ4dNg9Ge2dziPVFRPtmeLT84gC8qvSzQ-PAYyU6yGmxqR8LzynBHC1nSwEtGSrQmFOQxt3-V7AsQF_fhhVp_k8V0Fn_1puBTyqOFg-CgZtcCvLrAD-w5jfYeulkPljDR2mQTUXpwhBRdkI7_NnDBCONjLzl9F5Ra&attredirects=0)
