Concepto de cliente y servidor
------------------------------

Una aplicación puede pensarse desde la óptica del

-   cliente: el que realiza pedidos
-   servidor: el que responde a esos pedidos

La separación puede ser:

-   lógica: ambos componentes residen en la misma máquina
-   lógica y física: además de pensarse como componentes separados el usuario utiliza un cliente en su máquina y accede al servidor que concentra esos pedidos y se encarga de responderlos.

¿Qué tipo de pedidos hace el cliente? Esto depende de la arquitectura sobre la cual trabajemos:

Arquitectura de las UI
----------------------

1.  Aplicación centralizada
2.  Aplicación distribuida (Cliente/Servidor)

### Aplicación centralizada

El cliente tiene poca o nula inteligencia. El servidor tiene muchas responsabilidades, esto es: - recibe los parámetros del cliente, los valida y los transforma - procesa las acciones de negocio - transforma los resultados de esas acciones y - genera la visualización que va a obtener el cliente como respuesta. Del lado del cliente casi no hay lógica, ni de presentación ni de negocio.

Estos sistemas eran los preponderantes hasta mediados de los '80, la configuración tradicional era tener como clientes "terminales bobas" o programas que emulaban este comportamiento y un mainframe con grandes capacidades que actuaba como servidor.
