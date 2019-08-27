---
layout: article
title: Introducción a la Arquitectura web
categories: [web, arquitectura, intro]
featured: true
---

# Arquitectura distribuida

La arquitectura web trabaja con dos nodos:

- el **cliente** tiene un programa ejecutable (application client, el _web browser_ o navegador es el más común)
- y el **servidor** tiene otro programa ejecutable: en la materia será nuestro application server el que tendrá una VM donde vivan los objetos de negocio.

Estos nodos son lógicos: pueden estar ubicados físicamente en la misma máquina, pero igualmente tendremos una separación de componentes en cliente y servidor.

El cliente hace pedidos a través de un puerto contra el servidor, el servidor responde. El flujo de mensajes siempre comienza en el cliente:

- cliente pide servicio (request)
- servidor responde (response)

![image](/img/wiki/ui-web-arquitectura.png)

## Algunas consecuencias

- Nuestra aplicación pasa a ser una **aplicación distribuida**: va a tener una parte corriendo en el servidor y otra parte corriendo en el cliente. Dependiendo de la arquitectura que elijamos
  - podemos tener la mayor parte de la lógica en el servidor y tener un cliente liviano (thin) o ZAC (Zero Administration Client). Entonces lo que le llega al cliente es sólo un documento HTML, y es fácil mantener la aplicación cuando tengo muchos clientes ubicados
  - o bien podemos poner gran parte de la lógica en el cliente y utilizar la parte server solamente para sincronizar la información entre sesiones de usuario
- de todas maneras, por más liviano que sea el cliente, los _browsers_ no son uniformes, entonces si queremos que una aplicación ande en todos ellos muchas veces vamos a tener que manejar código específico para cada plataforma (browser, versión, sistema operativo y a veces hasta el hardware).
- como el cliente es el que dispara los pedidos, todas las interacciones entre el usuario y la aplicación deben ser iniciadas por el usuario, la aplicación no puede tomar la iniciativa. Ej: si tengo una lista de tareas pendientes, para que aparezca una nueva tarea hay que obligar al cliente a que dispare el refresh.

## Pedido/respuesta

Antes de meternos más de lleno, nos preguntamos: la tecnología de objetos ¿es consistente con la metáfora "pedido-respuesta" (request/response)? Sí, en definitiva es la representación de lo que es un mensaje.

Lo que pasa es que en un sistema con objetos no pongo restricciones: cualquiera puede ser emisor y cualquiera receptor. En cambio en la tecnología web siempre es el cliente el que pide y siempre el servidor el que responde.

# Cómo se implementa la comunicación

El cliente dice: "necesito x". Esto se traduce en una dirección de una página en particular, esa dirección recibe el nombre de **URL** (Uniforme Resource Locator, o forma de encontrar un recurso en el servidor):

```bash
http://localhost:8080/html-css/index.html
```

donde

- http es HyperText Transfer Protocol, el **protocolo de comunicación** por defecto que usan los navegadores
  - otros protocolos son https (donde los datos viajan encriptados), ftp, etc.
- localhost es el **servidor web** hacia el que vamos a conectarnos
  - en este caso `localhost` es el web server que está en la PC local, que equivale a la dirección IP 127.0.0.1
  - el servidor web puede ser una dirección IP o un nombre que luego es convertido a una dirección IP a través de un DNS (Domain Name Server)
- 8080 es el puerto donde el servidor está "escuchando" pedidos
- y finalmente la página que queremos cargar, que recibe el nombre de **recurso**

La forma en que publicaremos las páginas como **rutas** depende de la tecnología en la que trabajemos y lo veremos más adelante, lo importante es entender que una página html es accesible para un usuario con una ruta única llamada URL.

# HTTP

[Http](https://es.wikipedia.org/wiki/Protocolo_de_transferencia_de_hipertexto) es un protocolo **no orientado a conexión** que define la forma de comunicación entre el cliente y el servidor.

Recordemos que no-orientado a conexión significa que no guarda ninguna información sobre conexiones anteriores, por lo que no tenemos el concepto de sesión de usuario, es un protocolo sin estado (_stateless protocol_). Esto tiene varias implicancias, la más fuerte es que requiere que la aplicación mantenga la información necesaria para mantener una sesión (por ejemplo, sabiendo qué usuario es el que está haciendo una operación).

Un mensaje http tiene formato de texto, por lo que es legible al usuario y fácilmente depurable, como vemos en el siguiente video:

![video](/img/wiki/ui-web-architecture-http-request.gif)

Abrimos en un navegador las herramientas de desarrollo (por lo general es la tecla F12), y en la solapa _Network_ podemos inspeccionar las distintas respuestas que procesa el navegador, con el pedido http original que hace un mensaje de tipo `GET`.

## Tipos de mensaje

Un cliente puede enviar un pedido al servidor utilizando diferentes métodos

- `GET`: asociada a una operación de lectura, sin ningún otro efecto
- `HEAD`: es exactamente igual al pedido vía GET pero enviando únicamente el resultado de la operación en un header, sin el contenido o _body_
- `POST`: se suele asociar a una operación que tiene efecto colateral, no repetible
- `PUT`: está pensado para agregar información o modificar una entidad existente
- `DELETE`: se asocia con la posibilidad de eliminar un recurso existente
- `OPTIONS`: permite ver todos los métodos que soporta un determinado servidor web
- `TRACE`: permite hacer el seguimiento y depuración de un mensaje http (se agrega información de debug)
- `CONNECT`: equivalente a un `ping`, permite saber si se tiene acceso a un host

Más adelante volveremos sobre esto al estudiar REST. Ahora veremos la diferencia entre hacer un pedido mediante GET vs. POST.

### Envío mediante GET method

Aquí los parámetros viajan dentro de la URL como par `clave=valor`: 

```
http://www.appdomain.com/users?size=20&page=5
```

- `?` delimita el primer parámetro
- `&` delimita los siguientes parámetros

La ventaja de utilizar este método es que dado que http es un protocolo no orientado a conexión, podemos reconstruir todo el estado que necesita la página a partir de sus parámetros (es fácil navegar hacia atrás o adelante). Por otra parte es el método sugerido para operaciones sin efecto, que recuperan datos de un recurso.

Por otra parte, no es conveniente para pasar información sensible (como password o ciertos identificadores), algunos navegadores imponen un límite máximo de caracteres para estos pedidos y necesita codificar los caracteres especiales (p. ej. el espacio a `%20`) dado que el request solamente trabaja con el conjunto de caracteres ASCII.

### Envío mediante POST method

Los parámetros viajan en el BODY del mensaje HTML, no se ven en la URL del browser. Aquí no hay restricciones de tamaño para pasaje de información y tampoco se visualizan los parámetros en la URL del browser.

### GET vs. POST

La [recomendación W3C](https://www.w3.org/TR/html4/interact/forms.html#submit-format) (World Wide Web Consortium) dice que deberíamos usar 

- GET cuando sepamos que se trata de consultas que no van a tener efecto colateral (no habrá modificación en el estado del sistema)
- POST cuando sepamos que el procesamiento de la página causará una alteración del estado del sistema (al registrar el alquiler de una película, al modificar los datos de un socio o al eliminar un producto de la venta). Otros métodos posibles que veremos son PUT y PATCH, para modificaciones y alteraciones parciales, respectivamente.

# El camino de un pedido http

- el browser se conecta con el servidor a partir del dominio o IP (localhost = 127.0.0.1) y puerto
- se envía la petición al servidor en base a dirección, método, parámetros, etc.
- el servidor responde a ese pedido: esa respuesta es una nueva página con un código de estado HTTP:

  - 200 : OK
  - 401 : Unauthorized
  - 403 : Forbidden
  - 404 : Not Found
  - 405 : Method not allowed
  - 500 : Internal Server Error

El lector puede buscar la lista de [códigos de error HTTP](https://es.wikipedia.org/wiki/Anexo:C%C3%B3digos_de_estado_HTTP) (las especificaciones [RFC 2616](https://tools.ietf.org/html/rfc2616) y [RFC 4918](https://tools.ietf.org/html/rfc4918)) y formas de resolverlos.

- la aplicación cliente o _user agent_ se desconecta del servidor una vez procesada la respuesta

# Consecuencias del mensaje http para las aplicaciones web

La página es la mínima unidad de información entre cliente y servidor, lo que implica:

- **problemas en la performance**: no siempre debería refrescar toda la página si sólo necesito actualizar parcialmente la información de dicha página
- **problemas en el diseño**: tengo dificultades para poder particionar una pantalla en componentes visuales
- **problemas de usabilidad**: para que la página sea dinámica necesitamos forzar una comunicación con el servidor

**String oriented programming**: la comunicación entre cliente y servidor involucra solo texto, necesitamos adaptar fechas, números, booleanos y también los objetos de negocio (socios de un videoclub, alumnos, materias, vehículos de una flota, etc.) así como las colecciones.

# Procesamiento de la respuesta en el cliente

El servidor contesta con un string que tiene

- un _header_ donde indica el resultado del pedido
- un contenido, que forma parte del body, que puede ser HTML, json o cualquier otro formato que el cliente entienda

![](/img/wiki/ui-web-architecture-http-response.gif)

<br>
Arriba vemos la respuesta del navegador al buscar "Cuarteto de Nos" y abajo cómo procesa la consulta el cliente Postman.
<br>

![](/img/wiki/ui-web-architecture-http-response2.gif)

# Resumen

- Muchos clientes se conectan a un único servidor a través del protocolo HTTP
- En el cliente corre un programa llamado _user agent_ que es el encargado de establecer la comunicación con el web server. Se utiliza el concepto de thin client, es decir que se minimiza la lógica en los clientes, tendiendo a concentrarla en el servidor. **Consecuencias:**
  - se simplifica la utilización de la aplicación desde múltiples clientes sin costo de instalación ni mantenimiento.
  - se simplifica la lógica al mantener toda la aplicación centralizada.
- La descripción de las pantallas se basa en [HTML](html.html). Es decir, una página dinámica es un programa que genera un String conteniendo código HTML.
- Progresivamente se ha incrementado la tendencia a describir las cuestiones estéticas utilizando [CSS](css.html), por lo tanto la descripción de una vista estará dada por una combinación de HTML y CSS.
- La comunicación entre el cliente y el servidor está dada en la forma de pedido-respuesta (request-response).
- Todas las interacciones entre el usuario y la aplicación deben ser iniciadas por el usuario, la aplicación no puede tomar la iniciativa.
- La respuesta para cada pedido es una página nueva, la mínima unidad de comunicación entre el cliente y el servidor es una página. Esto tiene consecuencias tanto de performance como de usabilidad y también de diseño. Existen muchos tipos de pedido pero los más usuales son GET y POST. Los dos puntos anteriores limitan gravemente la posibilidad de utilizar mecanismos de binding.
- Cada pedido es independiente de los anteriores, es decir, la tecnología no provee de un soporte directo para manejar el estado de la conversación entre ambos procesos (**stateless**). Para modelar procesos que requieran de una comunicación más poderosa que esa deberán proveerse herramientas adicionales, frecuentemente manipuladas ad-hoc.

# Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
- [W3 - Web Architecture](https://www.w3.org/standards/webarch/)
