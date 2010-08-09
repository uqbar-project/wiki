Concepto de cliente y servidor
------------------------------

Una aplicación puede pensarse desde la óptica del

-   **cliente**: el que realiza pedidos
-   **servidor**: el que responde a esos pedidos

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

En este tipo de aplicaciones, se asume que

-   los clientes tienen más capacidad de procesamiento, por lo tanto suelen presentar una interfaz de usuario más "rica" que las aplicaciones centralizadas (superiores también a la de los browsers sin plugins ni elementos adicionales)
-   la comunicación hacia el servidor suele ser un cuello de botella, así que se trata de minimizar la cantidad de información a pasar entre cliente y servidor. Por lo general el cliente hace un pedido, la consulta se procesa en el servidor y vuelve la información procesada para que el cliente la presente en un formato amigable.
-   las aplicaciones distribuidas son más complejas arquitecturalmente: se dividen en 2, 3 hasta n niveles
-   como los clientes suelen estar en distintos puntos geográficos las actualizaciones de software suelen ser un dolor de cabeza

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

Links relacionados
------------------

-   [Algo3 Temario](algo3-temario.html)

