**self** es una pseudovariable que **siempre** referencia al objeto receptor del mensaje que hizo que se evalúe el método que estamos analizando en un momento dado. Tené en cuenta que no es posible asignar self, esta referencia es manejada por la máquina virtual, no por el programador.

Sirve para que el objeto receptor pueda mandarse un mensaje a sí mismo dentro de la implementación del método que se está ejecutando o para que, al mandar un mensaje a otro objeto que necesite colaborar con él, pueda pasarse a sí mismo por parámetro.

Ejemplos

Si tenemos este envío de mensaje:

` `**`Smalltalk`**
` pepita vola: 10`

` `**`Wollok`**
` pepita.vola(10)`

y el método que va a ejecutar pepita es

` `**`Smalltalk`**
` vola: unosKms`
`   self energia: self energia - unosKms`

` `**`Wollok`**
` method vola(unosKms){`
`   self.energia(self.energia() - unosKms)`
` }`

Cuando se le mande el mensaje para volar 10 kilómetros a pepita, se va a ejecutar el método que envía los mensajes para obtener y modificar la energía a self, o sea que pepita va a ser quien reciba ambos mensajes.

Como se mencionó antes, en cualquier método es perféctamente válido parametrizarse a uno mismo, por ejemplo:

` `**`Smalltalk`**
` teEntrena: unEntrenador`
`   self come: 50.`
`   unEntrenador entrenaA: self.`

` `**`Wollok`**
` method teEntrena(unEntrenador) {`
`   self.come(50)`
`   unEntrenador.entrenaA(self)`
` }`
