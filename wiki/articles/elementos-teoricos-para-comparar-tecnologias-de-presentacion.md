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
-   Restringe las posibilidades de uso de la máquina a lo provisto por el application client. Por ejemplo en el caso de una aplicación web, tradicionalmente las aplicaciones estuvieron limitadas a la utilización del lenguaje HTML, lo que limitaba en gran medida las posibilidades de interacción con el usuario.

### Evolución de los tipos de cliente

Con el tiempo la clasificación taxativa entre clientes pesados y livianos se fue diluyendo y fueron apareciendo opciones intermedias. Desde ambos lados fueron apareciendo herramientas que intentaban incorporar en uno de los mundos algunas de las ventajas del otro.

En un primer lugar la universalización del concepto de Virtual Machine o la popularización de diferentes lenguajes interpretados hace que sea difuso cuándo una aplicación es *stand alone* o está utilizando un application client. Por ejemplo, una aplicación Java puede verse como un cliente liviano que se ejecuta sobre un application cliente preinstalado (la JVM) o bien ver a ambos como un programa instalable único. En definitiva es una cuestión de como se distribuye el programa, ya que hoy en día no existen practicamente lenguajes que no requieran de un intérprete, máquina virtual o determinadas bibliotecas instaladas previamente para poder ejecutar programas.

El cuadro se completa cuando se incorporan herramientas actualizar código dinámicamente en lenguajes tradicionalmente pensados para aplicaciones pesadas como Java (*Java WebStar*).

Inclusive la utilización de application clients se ha extendido a nuevos entornos, entre ellos podemos mencionar dos: Firefox y Eclipse, en ambos casos encontramos un entorno base o microkernel que provee de un entorno para la ejecución de aplicaciones y una arquitectura basada en *plugins* o *add-ons*, que son los que en última instancia dan forma a la aplicación.

Por otro lado la popularización de las aplicaciones en Internet se contrapone con la gran cantidad de limitaciones que impone el HTML como lenguaje base para modelar las interfaces de usuario de dichas aplicaciones. Eso fue dando lugar a la aparición de múltiples tecnologías que intentan sobre ponerse a dichas limitaciones, algunos ejemplos son:

-   La posibilidad de ejecutar JavaScript dentro del web browser permite tener comportamiento en el cliente que ya no se delega en el servidor.
-   Manipular los componentes visuales desde ese código JavaScript en el cliente, para salir de las limitaciones impuestas por el HTML.
-   La incorporación de tecnologías como AJAX permiten romper la metafora navegacional definida originalmente por el browser.

Adicionalmente, la aparición de herramientas como Flash, Applets, SVGs y HTML5 incorporan nuevos application clients que rompen la visión original del browser-intérprete-de-HTML.

A las aplicaciones que salen de las limitaciones de navegación, interacción y visuales que tenían las aplicaciones web tradicionales se las denomina **Rich Internet Applications (RIA)** y por extensión también a las tecnologías que permiten desarrollar ese tipo de aplicaciones (se podría sumar también JavaScript a esta lista). Si bien se podría decir que este tipo de ideas están aún en evolución, se observa una tendencia a tener application clients cada vez más poderosos.

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

