Arquitectura usual de una aplicación de software
------------------------------------------------

Para generar una aplicación, podemos dividir lógicamente los componentes según el rol que juegan dentro del sistema en sí mismo:

-   **Presentación**: son elementos que trabajan con la [interfaz de usuario](definiciones-iniciales-de-ui--que-es-una-interfaz-de-usuario-.html), dependientes de la tecnología

<!-- -->

-   **[Dominio](definiciones-iniciales-de-ui--que-es-el-modelo-de-dominio-.html)**: son las abstracciones que tienen significado para el que conoce el negocio (una factura, un alumno, un proveedor, una encuesta de satisfacción de servicio, etc.)

<!-- -->

-   **Persistencia**: son elementos que trabajan en almacenar y recuperar la información del sistema en un medio que persista en el tiempo (que no sea volátil).

Formas de división de responsabilidades entre cada una de esas partes
---------------------------------------------------------------------

Esta división de responsabilidades se suele denotar como capas de una aplicación. Aquí preferimos utilizar el término "concern" o al menos aclarar que la división no implica separación física de los componentes. La clasificación en presentación, dominio y persistencia tiene que ver con el objetivo que cumple cada componente dentro de la aplicación.

Interacción entre la UI y el dominio del sistema
------------------------------------------------

Otros links relacionados
------------------------

[Algo3 Temario](algo3-temario.html)
