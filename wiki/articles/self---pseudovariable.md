**self** es una pseudovariable que siempre referencia al objeto receptor del mensaje que hizo que se evalúe el método que estamos analizando en un momento dado.

Ejemplos:

`('hola' size) + 1`

El primer mensaje es **\#size** que se le envía al objeto `'hola'`, cuando estamos analizando la ejecución del método **\#size** en este instante **self** apunta al objeto `'hola'`.

El segundo mensaje es **\#+** con el parámetro `1` que se le envía al objeto `4` (o sea, lo que retornó `'hola'` `size`), cuando estamos analizando la ejecución del método **\#+** en este instante **self** apunta al objeto `4`.

Si tenemos este workspace:

` pepita vola: 10.`

y el método para vola: que va a ejecutar pepita es

` vola: unosKms`
`   self energia: self energia - unosKms`

Cuando se le mande el mensaje vola: al objeto referenciado por la variable pepita, se va a ejecutar el método que envía energia: y energia a self, o sea que pepita va a ser quien reciba ambos mensajes.
