Clientes livianos y pesados
---------------------------

### Introducción a los conceptos de cliente y servidor

Si una aplicación es accedida por múltiples usuarios simultáneamente, probablemente cada uno de ellos lo haga desde su propia computadora en forma remota. Al programa que se ejecuta en la máquina del usuario se lo denomina **cliente**.

Luego, esos clientes necesitarán poder compartir información entre sí; en muchos casos esa información se centralizará en una o más programas que se denominan **servidor** (en inglés *server*).

La forma de repartir responsabilidades entre clientes y servidores dan lugar a un primer nivel de clasificación de las posibles arquitectura de una aplicación. Desde el punto de vista de la teoría de presentación, el punto más interesante será el cliente, ya que será el que tenga la responsabilidad de interactuar con el usuario.

### Clasificación tradicional

Tradicionalmente se solía clasificar a los clientes entre **pesados** y **livianos**.

Según esa clasificación, un cliente pesado tiene las siguientes características:

-   Se *instala* en la máquina cliente.
-   Es un programa independiente y completo (en inglés *stand alone*, esto es, no requiere de la presencia previa de otros programas en la máquina en la que se va a ejecutar (como máquinas virtuales, intérpretes, application clients, etc).
-   Está pensado para ser utilizado en un entorno de ejecución específico (hardware, sistema operativo, etc) y eso le permite aprovechar todos sus recursos sin limitaciones.
-   

<!-- -->

-   Características de los clientes "livianos" y "pesados"
    -   Capacidad de distribuir o centralizar la lógica.
    -   Actualización de versiones

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

