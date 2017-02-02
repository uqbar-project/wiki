---
layout: article
title: Atributos de calidad
---

Atributos de calidad (también cualidades del software) son características no funcionales que se consideran deseables en un sistema de software. Sin embargo, no todos los sistemas de software deben tener en cuenta todos estos atributos o cualidades, algunas serán más importantes que otras dependiendo del sistema, y ciertamente no se pueden maximizar todas a la vez.

Se establece una diferencia entre cualidades y requerimientos, porque algunas de ellas pueden incorporarse como entrada al diseño por un camino distinto al del análisis (por ejemplo, como restricciones de arquitectura o influencias del entorno).

Simplicidad
-----------

Simplicidad es la ausencia de complejidad o dificultades. En el desarrollo de software puede resultar de interes diferenciar entre complejidades esenciales y accidentales.

-   **Complejidad esencial:** las que son propias o intrínsecas al problema que se desea solucionar. Es natural que un problema complejo tenga soluciones con algún grado de complejidad.
-   **complejidades accidentales:** aquellas que surgen por malas decisiones de diseño. Naturalmente, se intentará evitar diseñar soluciones que sean más complejas de lo que el problema requiere.

Determinar si una dificultad en un diseño o programa es esencial o accidental, nos permite atacar las dificultades accidentales, buscando soluciones más simples.

Correctitud, consistencia, completitud
--------------------------------------

Correctitud:Ausencia de errores.
Consistencia:Coherencia entre las operaciones que realiza el usuario.
Completitud:Capacidad del sistema para realizar todas las operaciones que usuario podría requerir.  

Un artículo interesante sobre correctitud consistencia y completitud: [Worse is better](http://www.jwz.org/doc/worse-is-better.html)

Robustez
--------

Robusto es un sistema que goza de buena salud y que brinda garantías de que va a continuar teniendo buena salud. Algunos síntomas de un sistema robusto son:

-   la capacidad de ser modificado sin introducir errores (opuesto a *error prone*)
-   durabilidad del sistema funcionando correctamente (no aparecen errores aleatorios)

Diferentes usuarios tendrán diferentes visiones de la robustez del sistema.

Flexibilidad
------------

También llamada *modificabilidad*, es la capacidad para admitir cambios que pueden ser necesarios tanto por un cambio de requerimientos como por la detección de un error que debe ser corregido. Una variante de flexibilidad es la *extensibilidad*, es decir, la posibilidad de agregar nuevos requerimientos.

Performance
-----------

La performance es una medida de la eficiencia en el uso de recursos del sistema ejecutándose, por ejemplo:

-   Uso de procesador
-   Memoria
-   Almacenamiento permanente (discos rígidos, etc).
-   Uso de redes
-   ... o cualquier otro recurso físico.

Escalabilidad
-------------

Es la capacidad de un sistema para trabajar con diferentes cantidades de trabajo, como cambios en el volumen de datos o flujo de pedidos. Con frecuencia se estudia la escalabilidad de un sistema *hacia arriba*, es decir, se mide la capacidad del sistema para manejar, por ejemplo, un mayor volumen de datos. La medida de escalabilidad no requiere que el sistema funcione intacto en las nuevas condiciones, en cambio es una medida de la facilidad con la que se lo puede adaptar al nuevo entorno, por ejemplo, si está preparado para que yo agregue un servidor más a un *cluster* eso se podría considerar escalable.

También puede ser de utilidad analizar la flexibilidad *hacia abajo*, es decir, la posibilidad de un sistema de adaptarse a un entorno más sencillo. En estos casos, se analiza, por ejemplo, la posibilidad de evitar el uso de recursos que encarecen el sistema y podrían no ser indispensables, por ejemplo ejecutar toda la aplicación en un único servidor en lugar de cada *capa* en uno distinto o bien reemplazar determinados componentes adquiridos por otros de menor costo de licencia.

Un error común es confundir escalabilidad con extensibilidad.

Seguridad
---------

Algunas visiones de la seguridad son:

-   Comprobar la identidad de las personas que intentan acceder al sistema.
-   Garantizar que sólo las personas específicamente autorizadas pueden ver determinada porción de la información del sistema
-   Garantizar que sólo las personas específicamente autorizadas pueden modificar determinada porcióń de la información del sistema o bien realizar determinadas acciones.

Usabilidad
----------

La facilidad con la que el sistema o componente se puede utilizar o bien aprender a utilizar.

Constructibilidad
-----------------

La constructibilidad es una medida inversa a la complejidad de la construcción del sistema. Las decisiones de diseño pueden afectar severamente la dificultad para construir ese sistema.
