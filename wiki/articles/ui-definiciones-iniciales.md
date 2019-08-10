---
layout: article
title: Definiciones iniciales de ui
categories: [ui, dominio, concerns]
featured: true
---

# Interfaz de usuario

Es todo lo que permite a un usuario interactuar con el sistema. Una interfaz se implementa por lo general con una pantalla, pero no es el único dispositivo posible.

Es todo lo que permite a un usuario interactuar con el sistema: esto incluye componentes lógicos (software) y físicos (hardware). Interfaz de Usuario también se puede abreviar por sus siglas en inglés: [**UI (User Interface)**](https://en.wikipedia.org/wiki/User_interface) y en general se suele hablar de la interfaz gráfica como la parte de **presentación** de una aplicación.

# Objetivo de una materia que enseña UI

Naturalmente, el objetivo de la materia es aprender a programar y diseñar interfaces de usuario. Sin embargo, la construcción de interfaces de usuario presenta muchos aspectos distintos y por eso conviene entender en cuáles vamos a hacer foco.

## Diseño gráfico y diseño de sistemas

En primer lugar, cuando decimos "diseño", nos referimos tanto al diseño de software como al diseño gráfico:

- el diseño de sistemas se preocupa por distribuir correctamente las responsabilidades de los componentes que forman una aplicación, para lo cual necesitamos aplicar los conceptos y patrones de diseño que previamente incorporamos
- el diseño gráfico de una aplicación trabaja la disposición del contenido, la elección de colores (principal, secundario, de fondo, etc.), la iconografía, logos, imágenes, la _usabilidad_ de manera que sea cómoda e intuitiva para el usuario y ofrezca un mínimo de resistencia o de adaptación.

Si bien una materia del ámbito de las TIC está más relacionada con el primer punto, no es menor la preponderancia que adquiere últimamente el diseño gráfico, ya sea porque es una responsabilidad directamente delegada a quien construye la UI o bien constituye una actividad interdisciplinaria con un especialista.

Nuestro objetivo también es concentrarnos en producir aplicaciones que se destaquen por sus cualidades de mantenibilidad, flexibilidad, claridad, robustez, entre otras. En el ámbito específico de las interfaces de usuario, un criterio importante será separar la responsabilidad correspondiente a la interfaz de usuario de la lógica de dominio. Repasemos entonces de qué hablamos cuando hablamos de "dominio" o "negocio".

# Modelo de dominio

Cuando le pedimos al sistema que haga algo, hay reglas que rigen el negocio que manejamos.

Si el cliente sólo puede pagar con cheque a 30/60/90 días, hay una regla de negocio que lo dice. Si un alumno no puede anotarse en un final porque debe una correlativa, hay otra regla de negocio que lo dice. Si un empleado cobra un 10% del sueldo básico por presentismo, hay otra regla de negocio que lo dice.

Lo que forma parte del dominio de mi aplicación es encontrar

- un cliente que tenga un método

``` java
public void pagar(TipoPago tipoPago, BigDecimal monto)
```

donde se resuelva esa responsabilidad

- un alumno que tenga un método

``` java
public void inscribirseAFinal(Materia materia)
```

donde se resuelva esa responsabilidad

- etc.

O sea,

- si programamos con objetos, el modelo de dominio se compone de objetos con responsabilidades y relaciones que permiten definir los casos de uso del negocio.
- si programamos en otro paradigma, el modelo de dominio serán las entidades + los procesos que resuelven las cosas que necesito para la aplicación.

## Complejidades del dominio y de la UI

Pongamos por caso esta interfaz conocida de Twitter:

![twitter](/img/wiki/ui-twitter.gif)

El caso de uso "Twittear" parecería no tener una UI tan compleja, aun así, la página chequea todo el tiempo la cantidad de caracteres que escribo y en una forma gráfica está mostrando el estado de mi tweet, e incluso debe deshabilitar el botón de "Enviar tweet" si excedo el máximo permitido.

Por otra parte, una vez validado, el mensaje que recibe el negocio es algo como:

```java
twitter.agregarTweet(nuevoTweet)
```

que debería incorporar ese nuevo tweet a la colección (la persistencia requiere de un objeto arquitectural que normalmente está fuera del alcance del objeto de dominio).

En general, la lógica de la presentación **suele ser siempre mucho más compleja** que la del negocio, aun cuando el negocio pueda (y debería) ayudar a la UI a agregar funcionalidades que mejoren la experiencia de usuario. 

> Corolario: no es verdad que si el dominio está bien construido la presentación se hace sola, y deberíamos considerar que la gente con más experiencia en un proyecto debería estar a cargo de la interfaz de usuario (algo que lamentablemente no siempre sucede)

# ¿Qué objetivos nos proponemos al programar una interfaz de usuario?

Por supuesto que ande, pero además vamos a priorizar ciertas cualidades de diseño: mantenibilidad, flexibilidad, claridad, robustez, entre otras. En particular **tratar de no mezclar ideas de presentación con negocio**. O sea, separar la lógica para definir la interacción con el usuario y la lógica propia del dominio. ¿Por qué?

- porque no quiero que mi dominio se vea afectado por cuestiones tecnológicas.
- porque eso me lleva a perder cohesión en los objetos de presentación, que además de encargarse de mostrar la información tienen que atacar cuestiones de negocio. Entonces en dos pantallas distintas tengo que repetir la misma validación o el mismo comportamiento.
- porque tengo más restricciones a nivel usuario y tecnológicos del lado de la UI (es la parte más compleja y la menos madura, cuesta encontrar buenas abstracciones)

Volviendo al ejemplo de Twitter,

- saber qué color de fondo mostrar en el texto ==> es responsabilidad de la UI
- determinar qué texto está excedido del tamaño de un tweet ==> es responsabilidad del dominio
- saber la longitud de un tweet ==> es responsabilidad del dominio
- saber si un tweet está excedido ==> es responsabilidad del dominio
- saber si un tweet está cercano a excederse ==> es responsabilidad del dominio
- mostrar un color celeste, amarillo o rojo como indicador de que el tweet tiene el tamaño apropiado, está cercano a excederse o definitivamente se excedió ==> es responsabilidad de la UI

Mantener separados estos _concerns_ facilita el cambio de tecnologías de la UI, el testeo unitario de ese dominio, entre otras cosas.

# Conceptos básicos del diseño

- [Cohesión](conceptos-basicos-del-diseno.html#tocAnchor-1-4)
- [Acoplamiento](conceptos-basicos-del-diseno.html#tocAnchor-1-5)

¿Dónde interviene el acoplamiento al programar la UI? El componente de UI va a tener que conocer al componente que maneja la lógica de dominio, de otra manera la aplicación no va a funcionar. Pero tampoco es bueno que la interfaz defina lógica que es propia del cliente, o de la factura, o de un empleado o de un alumno (para más información ver [Interacción entre la UI y el dominio del sistema](integracion-de-la-ui-en-una-arquitectura-de-un-sistema-de-software.html)). Es cierto que agregar un atributo que el usuario deba visualizar o modificar a través de la interfaz fuerza inevitablemente a un cambio en la UI, pero cambios en la lógica de negocio no deberían necesariamente afectar la UI. Así que otro de nuestros objetivos será minimizar el acoplamiento, no por ser puristas, sino porque nos traerá como beneficio no vernos impactados por cualquier tipo de cambio.

# Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
