---
layout: article
title: Integracion de la ui en una arquitectura de un sistema de software
---

Arquitectura usual de una aplicación de software
------------------------------------------------

Para generar una aplicación, podemos dividir lógicamente los componentes según el rol que juegan dentro del sistema en sí mismo:

-   **Presentación**: son elementos que trabajan con la [interfaz de usuario](definiciones-iniciales-de-ui.html), dependientes de la tecnología

-   **[Dominio](definiciones-iniciales-de-ui.html)**: son las abstracciones que tienen significado para el que conoce el negocio (una factura, un alumno, un proveedor, una encuesta de satisfacción de servicio, etc.)

-   **Persistencia**: son elementos que trabajan en almacenar y recuperar la información del sistema en un medio que persista en el tiempo (que no sea volátil).

Formas de división de responsabilidades entre cada una de esas partes
---------------------------------------------------------------------

Esta división de responsabilidades se suele denotar como **capas** de una aplicación. Aquí preferimos utilizar el término **concern** o al menos aclarar que la división no implica separación física de los componentes. La clasificación en presentación, dominio y persistencia tiene que ver con el objetivo que cumple cada componente dentro de la aplicación.

Interacción entre la UI y el dominio del sistema
------------------------------------------------

Una idea bastante instalada en el mercado es abstraer la presentación del dominio tanto como sea posible. De hecho algunos piensan que lo mejor es que no se conozcan/ni se hablen.

Entonces la presentación habla con un objeto intermedio que no tiene comportamiento, solo alguno de los atributos a los que se accede mediante getters y setters. Como la característica de estos objetos es transferir informacion del dominio a la vista se los denomina **Data Transfer Objects o DTO**.

Un ejemplo podría ser: en la actualización de un empleado se ingresan nombre, apellido, DNI, fecha de ingreso y cargo. En lugar de que la vista conozca a un objeto Empleado que tenga métodos de negocio (por ejemplo que calcule la antigüedad, que sepa cuánto cobra, que conteste cuántas horas trabajó un mes, etc.) generamos un EmpleadoDTO, que tiene solamente nombre, apellido, DNI, sueldo mensual en base al cargo y antigüedad, éstos últimos dos atributos calculados. El EmpleadoDTO no sabe calcular su antigüedad, ni tiene relación con otros objetos. Esta técnica puede ser útil cuando estamos trabajando en ambientes distribuidos, es decir, en muchas VM que necesito sincronizar.

**Pero al separar la presentación y el negocio de esta manera poco feliz estoy metiendo una solución que para comunicar dos ambientes OO descarta las principales ideas del paradigma** (el objeto como un ente que agrupa atributos y comportamiento). Incluso es un problema porque tenemos varios objetos que están representando a un empleado:

-   el Empleado
-   el EmpleadoDTO

En definitiva pareciera que la vista no se mezcla con el negocio pero el [acoplamiento](conceptos-basicos-del-diseno.html) es claro: para saber cuándo fue la última vez que le pagué el sueldo necesito:

1.  generar un nuevo método de negocio si no lo tengo, pero también
2.  agregar un atributo al EmpleadoConsultaDTO para que la vista lo pueda mostrar

Nuestra idea es que la presentación no solo hable con el dominio sino que le pida todo lo que le tenga que pedir: en el ejemplo anterior sería muy bueno que la vista le pregunte directamente al empleado cuál fue la última vez que le pagué.

Links relacionados
------------------

-   [Temario Algoritmos III](algo3-temario.html)

