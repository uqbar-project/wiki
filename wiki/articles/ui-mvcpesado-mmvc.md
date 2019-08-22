---
layout: article
title: Application model. Extendiendo el MVC.
categories: [arena, viewmodel, presentationmodel, applicationmodel, mmvc, mvvm]
featured: true
---

# Introducción

En Arena cada vista requiere un modelo. Esto implica encontrar una abstracción que pueda cumplir esa responsabilidad. Algunas pantallas como el conversor de millas a kilómetros utilizan un objeto de dominio como modelo; pero cuando la complejidad de la interacción con el usuario crece, no nos alcanza con tratar de resolverlo con un objeto de dominio como Celular, Socio o Película.

Entonces nuestro objetivo es tener un objeto que sea totalmente independiente de la tecnología, pero que tenga todo el comportamiento necesario de la aplicación. Es la representación del comportamiento global de la aplicación sin la componente tecnológica.

# Algunos ejemplos

Una pantalla de búsqueda de clientes de una compañía que vende celulares:

![image](/img/wiki/mmvc-ejemplo1.png)

Una ventana para crear un pedido, que selecciona cliente y producto (y también permite darlos de alta):

![image](/img/wiki/mmvc-ejemplo2.png)

E incluso podemos pensar que una pantalla de alta o edición puede manejarse con un application model, esto permite no tener una referencia extra para conocer al home o repositorio:

# Objetivo

Consideramos importante la separación entre los componentes de la aplicación que dependen de la tecnología (vista, controller) y los que no (modelos de aplicación o de dominio). Y el application model nos da la herramienta para lograr eso.

Esta estrategia nos permite:

- Tener un modelo rico, en el cual poder programar y diseñar libremente, sin dependencias tecnológicas.
- Por ser independientes de la vista pueden ser testeados unitariamente con herramientas sencillas (las que ya conocen, sin la complejidad de las herramientas de testeo automático para interfaces de usuario, vean para eso los tests del ejemplo de los celulares).
- Por adaptarse a las necesidades de la vista, simplifican el mapeo que realizan los controllers.

El application model funciona como buffer entre la vista y el modelo de dominio y nos va a permitir construir esa parte de la aplicación programando con objetos y en nuestro lenguaje de preferencia, en lugar de tener que adaptarse a un framework, tecnología o lenguaje que no tiene la misma potencia. Algunas variantes de ese concepto se pueden ver en el artículo [formas de vincular una vista con el modelo de dominio](ui-mvcpesado-formas-de-vincular-una-vista-con-el-modelo-de-dominio.html).

# Objeto de dominio vs. application model

Podemos encontrar dos diferencias importantes:

- El application model tiene otra naturaleza en cuanto a la forma en que aparece, no está (a priori) entre los conceptos que maneja el usuario.
- Tiene otro ciclo de vida. Los objetos del dominio se crean, se guardan (utilizando probablemente un repositorio persistente), luego se pueden consultar, modificar, etc. En cambio los objetos de aplicación se usan una sola vez, suelen tener el estado conversacional entre un usuario y la aplicación, representan un caso de uso.

# Esquema MMVC

El objeto Application Model da origen al esquema MMVC [1]:

- V - Vista: la pantalla `*Window`
- C - Controller: adapta la vista con el modelo, en el caso de Arena son muchos objetos con responsabilidades bien definidas que se relacionan con el application model: binders, transformers, filters, etc.
- M - Modelo de la vista/modelo de aplicación/application model: modela al caso de uso, mantiene simple el binding con la vista
- M - Modelo de dominio

![image](/img/wiki/mmvc-grafico.png)
