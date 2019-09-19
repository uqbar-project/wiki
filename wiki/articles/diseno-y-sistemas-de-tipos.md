---
layout: article
title: Diseno y sistemas de tipos
---

En programación el chequeo estático de tipos permite detectar errores en un programa de forma previa a su ejecución. Existen amantes y detractores de esta idea y existen lenguajes profesionales con y sin chequeo de tipos. Quienes apoyan el uso de estos mecanismos sostienen que este tipo de chequeos garantiza o ayuda a garantizar la ausencia de errores en los programas, mientras que los que prefieren evitar estos chequeos sostienen que:

- a menudo resultan limitantes descartando soluciones que podrían ser viables
- que agregar la información de tipos a un programa es trabajoso y reduce su legibilidad y finalmente
- que las garantías que otorga un sistema de tipos pueden lograrse por otros mecanismos (como el testeo unitario).

¿Cómo afecta el chequeo de tipos a la actividad de diseño?
----------------------------------------------------------

- Si para diseñar utilizamos herramientas capaces de validarlos, entonces podrían ayudar a detectar errores. De estas herramientas podemos ver dos sabores:
  - Herramientas que permiten construir diagramas y realizan chequeos sobre esos diagramas.
  - Si utilizamos código, o pseudo-código como herramientas dentro del proceso de diseño, podemos utilizar los chequeos de tipos en esos programas para detectar problemas. Esto puede resultar particularmente importante al realizar rediseños y pruebas de rediseño sobre un programa ya construido.
- El sistema de tipos de la tecnología en la que se construye el programa diseñado establece restricciones sobre lo que se puede programar en ese lenguaje; entonces, si al diseñar no tengo en cuenta esas restricciones corro el riesgo de producir un diseño que luego no es fácil (o incluso no es posible) de construir en la tecnología elegida. El sistema de tipos forma parte del [metamodelo](metamodelo.html) del lenguaje de programación; uno puede elegir establecer una relación más o menos cercana entre ambos metamodelos, con las consecuencias ya explicadas.
- Finalmente, un tipo determina el conjunto de operaciones que se puede realizar sobre una entidad (entre otras cosas). Por entidad podemos entender cualquier porción de un sistema: objeto, clase, procedimiento, subsistema, tipo abstracto de datos, etc. Es posible establecer una relación directa entre las operaciones y las responsabilidades e interfaces asociadas a una parte de un sistema (que son objetivos específicos del diseño). Definir tipos es una forma de definir las interfaces entre entidades de software.
- Adicionalmente en sistemas de tipos con presencia de polimorfismo, un tipo no define la interfaz de una entidad sino que define un contrato que podrían cumplir múltiples entidades distintas.
