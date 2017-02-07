---
layout: article
title: Libreria y framework
---

Librería
--------

Una **librería** es un conjunto de funciones llamadas desde "afuera" por un cliente. Dentro del POO, esto es una clase que recibe un mensaje, lo ejecuta y devuelve luego el control al cliente. La instanciación de una librería es relativamente sencilla.

{% link_image Libreria.png %}

Framework
---------

El **framework** representa una abstracción de diseño y tiene un comportamiento en sí mismo. No es solamente una clase, sino que es un conjunto de objetos que se relacionan para servir a un dominio específico.

La integración con mi sistema se da de múltiples maneras

-   a veces extiendo de una superclase propia del framework,
-   o defino un objeto que respeta el contrato que pide el framework,
-   o envío un mensaje a un objeto del framework

pero es usual que el framework me pida luego cosas a mí, no tengo control sobre el flujo de envío de mensajes.

La instanciación del framework no es tan sencilla, ya que requiere un conocimiento del mismo.

{% link_image Framework.png %}
