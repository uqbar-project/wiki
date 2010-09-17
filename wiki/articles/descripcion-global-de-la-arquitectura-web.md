La programación web fue evolucionando a lo largo del tiempo

Características de la arquitectura web tradicional
--------------------------------------------------

-   Muchos clientes se conectan a un único servidor a través del protocolo HTTP
-   En el cliente corre un programa llamado browser o navegador que es el encargado de establecer la comunicación con el web server. Se utiliza el concepto de thin client, es decir que se minimiza la lógica en los clientes, tendiendo a concentrarla en el servidor.

**Consecuencias:**

-   -   se simplifica la utilización de la aplicación desde múltiples clientes sin costo de instalación ni mantenimiento.
    -   se simplifica la lógica al mantener toda la aplicación centralizada.
-   La descripción de las pantallas se basa en [HTML](html.html). Es decir, una página dinámica es un programa que genera un String conteniendo código HTML.
-   Progresivamente se ha incrementado la tendencia a describir las cuestiones estéticas utilizando [CSS](css.html), por lo tanto la descripción de una vista estará dada por una combinación de HTML y CSS.
-   La comunicación entre el cliente y el servidor está dada en la forma de pedido-respuesta (request-response).
-   Todas las interacciones entre el usuario y la aplicación deben ser iniciadas por el usuario, la aplicación no puede tomar la iniciativa.
-   La respuesta para cada pedido es una página nueva, la mínima unidad de comunicación entre el cliente y el servidor es una página. Esto tiene consecuencias tanto de performance como de usabilidad y también de diseño. Existen muchos tipos de pedido pero los más usuales son GET y POST. Los dos puntos anteriores limitan gravemente la posibilidad de utilizar mecanismos de binding.
-   Cada pedido es independiente de los anteriores, es decir, la tecnología no provee de un soporte directo para manejar el estado de la conversación entre ambos procesos (stateless). Para modelar procesos que requieran de una comunicación más poderosa que esa deberán proveerse herramientas adicionales, frecuentemente manipuladas ad-hoc.

Problemas de la programación web
--------------------------------

-   El estado conversacional se encuentra en el servidor.
-   Pedido -&gt; respuesta / servidor pasivo.
-   El HTML es texto.

Links relacionados
------------------

-   [Algo3 Temario](algo3-temario.html)

