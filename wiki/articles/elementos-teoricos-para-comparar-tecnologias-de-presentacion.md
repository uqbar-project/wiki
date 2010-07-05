Clientes livianos y pesados
---------------------------

### Introducción a los conceptos de cliente y servidor

Si una aplicación es accedida por múltiples usuarios simultáneamente, probablemente cada uno de ellos lo haga desde su propia computadora en forma remota. Al programa que se ejecuta en la máquina del usuario se lo denomina **cliente**.

Luego, esos clientes necesitarán poder compartir información entre sí; en muchos casos esa información se centralizará en una o más programas que se denominan **servidor** (en inglés *server*).

La forma de repartir responsabilidades entre clientes y servidores dan lugar a un primer nivel de clasificación de las posibles arquitectura de una aplicación. Desde el punto de vista de la teoría de presentación, el punto más interesante será el cliente, ya que será el que tenga la responsabilidad de interactuar con el usuario.

### Clasificación tradicional

Tradicionalmente se solía clasificar a los clientes entre **pesados** y **livianos**. Según esa clasificación, un cliente pesado tiene las siguientes características:

-   Se *instala* en la máquina cliente.
-   Es un programa independiente y completo (en inglés *stand alone*, esto es, no requiere de la presencia previa de otros programas en la máquina en la que se va a ejecutar (como máquinas virtuales, intérpretes, application clients, etc).
-   Está pensado para ser utilizado en un entorno de ejecución específico (hardware, sistema operativo, etc) y eso le permite aprovechar todos sus recursos sin limitaciones. Por ejemplo:
    -   Se pueden utilizar todos los periféricos de la máquina como ser impresoras o cualquier dispositivo específico.
    -   No hay limitaciones en cuanto a las formas de presentar la información o de interactuar con el usuario.
-   Contiene gran parte o toda la lógica de la aplicación, requiriendo pocos servicios del servidor, que se limita a ser un repositorio central de información.
-   Requiere grandes cantidades de recursos de la máquina cliente.

Los clientes livianos se caracterizan por delegar gran parte de la lógica de la aplicación a la máquina servidora y de esa manera requerir menor cantidad de recursos de la máquina cliente. Sin embargo hay otra característica de un cliente liviano que cambia radicalmente la forma de pensar las aplicaciones y es la aparición del concepto de **application client**.

El application client es un entorno en el cual se ejecutan las aplicaciones clientes de forma controlada, les da servicios que permiten simplificar la programación de las mismas y también tiene políticas que permiten proteger a la máquina de posible código malicioso. El application client más ampliamente conocido es el **web browser**, pero no es el único.

La ejecución dentro de un application client tiene gran impacto sobre las aplicaciones construidas de esta manera:

-   El application client se ocupa de obtener dinámicamente el código de la aplicación, eliminando la necesidad de instalación y de actualización de versiones.
-   La aplicación ya no es independiente y sólo puede ejecutarse en una máquina que contenga el application client correspondiente, esto nos lleva con frecuencia a no utilizar el application client que podría ser más útil a nuestros propósito sino a amoldarnos al que es más probable de encontrar instalado en las máquinas en las que querremos ejecutar nuestro programa.
-   Permite que la misma aplicación se ejecute en máquinas totalmente distintas, en tanto tengan el mismo application client.
-   Restringe las posibilidades de uso de la máquina a lo provisto por el application client.

### Evolución de los tipos de cliente

La línea divisoria tan firme.

-   Los clientes livianos van incorporando chiches de presentación.
-   Los clientes pesados también pueden actualizar versiones dinámicamente.
-   Ya no es tan cierto que los "livianos" consumen tantos recursos menos.
-   RIA

Descripción de la vista
-----------------------

-   Diferentes formas de describir una vista: programática, declarativa, visual, model driven.
    -   Diferencias entre diferentes variantes de declaratividad, scriptlets, etc.
    -   Manipulación de componentes vs. manipulación de texto.

Navegación
----------

-   Formas de navegación: pantallas y formularios, ventanas y diálogos (o SPI), manipulación directa.
    -   Control de la iniciativa: usuario o aplicación.
    -   Integración con el dominio de la aplicación: stateless (basada en servicios) o statefull (basada en objetos y en eventos).

Modelado
--------

-   Estado conversacional: en el cliente, en el server (sesión), en el pedido, en objetos específicos (por ejemplo: caso de uso).
-   Integración de la lógica: eventos y bindings (nivel de campo), formulario o pantalla (submit), objetos específicos (por ejemplo: caso de uso).
-   Arquitecturas: orientado a la presentación, datos, servicios, objetos.

