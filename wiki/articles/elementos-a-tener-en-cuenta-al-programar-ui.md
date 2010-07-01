Al trabajar con interfaces de usuario, aparecen muchos problemas nuevos que no suelen estar presentes en otro tipo de programas. En este módulo introductorio es hacer una enumeración (y definición) de algunos de esos problemas, con el objetivo de establecer una visión global de la problemática asociada a una interfaz de usuario, para ayudar a visualizar todas las cosas que deben tenerse en cuenta para producir una interfaz de usuario de calidad.

Dividiremos las actividades de una interfaz de usuario en dos grandes grupos. El primero de ellos plantea un modelo de ciclo de interacción, es decir, el conjunto de actividades que suelen ocurrir en cada interacción con el usuario, en secuencia:

-   Mostrar al usuario las distintas **acciones que puede realizar** (por ejemplo mediante un menú, botones, etc)
-   Permitir al usuario **iniciar una nueva tarea** o caso de uso.
-   Permitir al usuario **ingresar información** asociada a la tarea que desea realizar.
-   **Transformar** la información ingresada por el usuario al formato en el cual la aplicación los puede manipular internamente.
-   Actualizar el modelo de dominio con la información recibida.
-   **Validar** la información ingresada por el usuario. Suele haber tres puntos distintos de validación:
    -   Previa a la transformación.
    -   Posterior a la transformación y previa a la actualización del modelo.
    -   Luego de actualizado el modelo.
-   Ejecutar de una **acción de dominio** con la información recibida.
-   Manejar y presentar los **errores** si los hubiera.
-   Seleccionar la **siguiente vista** que se presentará al usuario y la información a mostrar en la nueva vista (por ejemplo, filtrado y/o proyección).
-   **Transformar los resultados** al formato en que pueden ser presentados al usuario.

En forma transversal a este circuito básico se presenta un segundo conjunto de actividades, que puede pensarse como transversal al primero:

-   Navegación de la aplicación entre las diferentes acciones que permite realizar.
-   Manejo de transacciones, concepto de transacción desde el punto de vista de la aplicación.
-   Manejo de la sesión de usuario, es decir, en el contexto de una aplicación multiusuario, la información relativa a la actividad de cada uno de los usuarios.
-   Internacionalización.
-   Distintas formas de verificación.

Criterios de calidad
--------------------

### Con respecto a la construcción:

-   Consistencia, robustez.
-   Simplicidad, claridad.
-   Desacoplamiento entre dominio y UI
-   Desacoplamiento entre lógica y tecnología
-   Testeabilidad o verificabilidad.

### Con respecto a la usabilidad

-   Claridad
-   Consistencia.
-   Riqueza (richness)
-   Feedback
-   Perfomance y percepción de performance.
-   Intuitividad, autoaprendizaje.

