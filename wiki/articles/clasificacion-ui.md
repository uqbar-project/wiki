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

El cliente tiene poca o nula inteligencia. El servidor tiene muchas responsabilidades, esto es:

-   recibe los parámetros del cliente, los valida y los transforma
-   procesa las acciones de negocio
-   transforma los resultados de esas acciones y
-   genera la visualización que va a obtener el cliente como respuesta.

Del lado del cliente casi no hay lógica, ni de presentación ni de negocio.

Estos sistemas eran los preponderantes hasta mediados de los '80, la configuración tradicional era tener como clientes "terminales bobas" o programas que emulaban este comportamiento y un mainframe con grandes capacidades que actuaba como servidor.

### Aplicaciones distribuidas

Conforme mejoraron las capacidades de procesamiento y bajaron los costos, los clientes comenzaron a tener más funcionalidades en el armado de una aplicación. Surgieron entonces:

-   las aplicaciones de dos niveles: cliente / servidor, donde el cliente generalmente tenía la responsabilidad de procesar consultas desde el servidor y manejar una interfaz de usuario "rica" (orientada a componentes visuales)
-   las aplicaciones de 3 y hasta n niveles: cliente / middleware / servidor

Las dificultades de tener aplicaciones distribuidas es el alto costo de mantenimiento del software que corre en los clientes cuando estos son muchos o están diseminados geográficamente en distintos puntos. La comunicación con el servidor suele ser por lo general un cuello de botella difícil de soslayar. Por otra parte, las capacidades gráficas y visuales de aplicaciones con clientes "ricos" son insuperables si la comparamos con las prestaciones que ofrece un browser genérico (sin agregados ni plug-ins).

Tipos de cliente
----------------

La aplicación que corre en un cliente puede ser entonces:

-   un browser, o navegador, que tiene capacidades limitadas por la tecnología: sabe "renderizar" -mostrar- controles HTML, ejecutar código en el cliente a través de javascript o algún lenguaje propietario del browser y enviar peticiones al servidor (generalmente redirigir el control a otra página con ciertos parámetros). El límite está demarcado por el lenguaje de hipertexto pensado para trabajar con información documental, no para aplicaciones de negocio.
-   un programa específicamente designado para tal fin, construido como parte de un sistema integral (un ejecutable hecho para un sistema operativo, o un programa pre-compilado)

RIA
---

Como hemos visto, los clientes pueden tener más o menos capacidades de procesamiento local y de "oferta visual" en la generación de la interfaz de usuario. Esto los divide en clientes pesados y livianos, respectivamente.

En los últimos tiempos los clientes livianos fueron incorporando tecnologías y herramientas para tratar de aumentar las funcionalidades del lado del cliente:

-   recarga selectiva de una página (ej: si hago una consulta anidada de combos, refrescar sólo el combo "hijo" que depende del "padre")
-   manejo de estado conversacional entre cliente y servidor que permita recolectar y actualizar la info sobre el repositorio remoto.

Entonces el cliente hace más que "mostrar en forma amistosa" el formulario... de hecho se acerca al formato cliente-servidor que hablábamos antes. Este híbrido de cliente liviano que incorpora funcionalidades propias de los clientes pesados generan las Rich Internet Applications, o RIA.

Otros links relacionados
------------------------

[Algo3 Temario](algo3-temario.html)
