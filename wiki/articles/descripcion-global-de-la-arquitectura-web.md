Características de la arquitectura web tradicional
--------------------------------------------------

-   Muchos clientes conectándose a un único servidor, comunicados utilizando el protocolo HTTP.
-   Utiliza el concepto de thin client, es decir que se minimiza la lógica en los clientes, tendiendo a concentrarla en el servidor. Consecuencias: se simplifica la utilización de la aplicación desde múltiples clientes sin costo de instalación ni mantenimiento.

`         o Simplifica la lógica al mantener toda la aplicación centralizada.`

`   * La descripción de las pantallas se basa en HTML. Es decir, una página dinámica es un programa que genera un String conteniendo código HTML.`
`         o Progresivamente se ha incrementado la tendencia a describir las cuestiones estéticas utilizando CSS, por lo tanto la descripción de una vista estará dada por una combinación de HTML y CSS.`

`   * La comunicación entre el cliente y el servidor está dada en la forma de pedido-respuesta (request-response).`
`         o Todas las interacciones entre el usuario y la aplicación deben ser iniciadas por el usuario, la aplicación no puede tomar la iniciativa.`
`         o La respuesta para cada pedido es una página nueva, la mínima unidad de comunicación entre el cliente y el servidor es una página. Esto tiene consecuencias tanto de performance como de usabilidad y también de diseño.`
`           Existen muchos tipos de pedido pero los más usuales son GET y POST.`
`         o Los dos puntos anteriores limitan gravemente la posibilidad de utilizar mecanismos de binding.`

`   * Cada pedido es independiente de los anteriores, es decir, la tecnología no provee de un soporte directo para manejar el estado de la conversación entre ambos procesos (stateless).`
`     Para modelar procesos que requieran de una comunicación más poderosa que esa deberán proveerse herramientas adicionales, frecuentemente manipuladas ad-hoc.`
