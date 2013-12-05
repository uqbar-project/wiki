Resumen
-------

El mecanismo de observer es así:

-   Hay un observador (que entiende el mensaje `update:`, y hay que implementarlo)
-   Hay un observado (que entiende los mensajes `changed:` y `addDependent:`, que no hay que implementar pero sí usar)

Inicialización
--------------

Al inicializar todo (antes de que empiece a funcionar) tenés que hacer:

`observado addDependent: observador.`

Uso
---

Después, el observado necesita avisar que él mismo cambió, y eso lo hace con:

`self changed: #variableQueCambió`

Pharo mágicamente le va a mandar el mensaje `update:` `#variableQueCambio` a todos sus observadores. Entonces, en cada observador tenés que definir el método:

`update: nombreDeLoQueCambio`
`   "acá hago lo que sea que quiera hacer el observer."`

Algo común para hacer ahí es chequear si me interesa ese `nombreDeLoQueCambio`, ó bien hacerle preguntas a mi observado (que me guardé de antemano).

Diagrama de Clases
------------------

<img src="ObserverPharo.png" title="ObserverPharo.png" alt="ObserverPharo.png" width="800" />
