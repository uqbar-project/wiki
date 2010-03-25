**self** es una pseudovariable que siempre referencia al objeto receptor del mensaje que hizo que se ejecutará el método que estamos analizando en un momento dado.

Ejemplo:

`('hola'` `size)` `+` `1`

El primer mensaje es **\#size** que se le envía al objeto `'hola'`, cuando estamos analizando la ejecución del método **\#size** en este instante **self** apunta al objeto `'hola'`.

El segundo mensaje es **\#+** con el parámetro `1` que se le envía al objeto `4` (o sea, lo que retornó `'hola'` `size`), cuando estamos analizando la ejecución del método **\#+** en este instante **self** apunta al objeto `4`.
