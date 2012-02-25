Introducción
------------

Las pruebas unitarias son una herramienta fundamental en el desarrollo de software. Con ellas es posible:

-   Encarar refactors y correcciones con mayor seguridad
-   Mantener una especificación viva del sistema
-   Confirmar la presencia de errores del sistema
-   Guiar nuestro diseño (si se emplea TDD)

El poder de las pruebas radica en que son, justamente, código (y no, por ejemplo, una especificación en un bibliorato), lo cual las vuelve facilmente automatizables. Y como se encuentra normalmente junto con el código productivo, los cambios en las especificaciones del mismo serán detectados rápidamente.

Sin embargo, al tratarse de código, un desarrollo poco cuidado de los casos de pruebas puede introducir problemas similares a los que surgen con el código productivo en los siguiente aspectos:

-   Expresividad: las pruebas se son ilegibles, los programadores no entienden lo que se está probando
-   Correctitud: las pruebas no validan lo que deberían, o, peor, lo hacen de forma incorrecta o no repetible.
-   Nivel de abstracción: las pruebas no tienen el nivel de abstracción suficiente, y repiten lógica, lo cual lleva a contar con muchos necesarios para una buena cobertura, pero casos extremadamente parecidos

Lo cual llevará a severos problemas de mantenibilidad de los tests y a eslóganes y prácticas tan aviesas como “test que se rompe, test que se elimina”. Así nuestra preciada cobertura desaparecerá sin avisar.

La moraleja es entonces que el nivel de calidad de las pruebas unitarias debe ser similar al del código productivo.

Nivel de abstracción justo
--------------------------

### Tests abstractos

### Aserciones personalizadas

### Delegar apropiadamente
