---
layout: article
title: Navegación
categories: [navegacion, cliente pesado, casos de uso]
featured: true
---

# Introducción

La navegación es un subconcepto de una incumbencia más general de nuestra aplicación, y en particular de nuestra interfaz de usuario: la **interacción**. Toda acción del usuario sobre la interfaz es una interacción. Navegación se refiere en particular a esas interacciones que el usuario realizará para poder llevar acabo diferentes **tareas**. En general serán todas las interacciones relacionadas con el ciclo de vida de una **tarea**:

- cómo **iniciar** una tarea
- cómo **terminar** una tarea:
  - exitósamente
  - cancelándola
- ¿será posible realizar tareas **en simultáneo**?
- ¿podrá el usuario **anidar tareas** (realizar una subtarea mientras aún sigue con la tarea más general)?

Cómo verán aquí, tanto nombrar el concepto de tarea les dará la idea de que es un concepto importante, ya que la navegación se define en base a ella. Pero entonces bajémoslo a algo concreto, ¿qué sería una tarea?

# Tarea / Caso de uso

Nos referimos a tarea aquí como una unidad de trabajo que comienza y realiza un usuario sobre el dominio.

Por ejemplo, en un sistema de gestión de un videoclub:

- Crear un nuevo usuario
- Alquilar una película
- Buscar un usuario
- Eliminar un usuario
- Crear una nueva película
- Buscar una película
- etc.

Esta definición tiene cierta similitud con la idea de caso de uso del análisis de requerimientos. Por eso a veces se utilizan ambos términos intercambiablemente.

Pero entonces, ¿cómo se relacionan las tareas con la UI?

# Relación entre una tarea y los elementos de UI

La relación entre las tareas y la UI es justamente la navegación.  Esta navegación, o su implementación va a depender de varios factores:

- La arquitectura: por ejemplo,
  - Aplicación standalone de escritorio
  - Aplicación web distribuida
  - Aplicación móvil distribuida
- Los dispositivos:
  - PC
  - Handhelds: PDA, Palm
  - Mobile: android
  - Dispositivos dedicados o de ciertas industrias particulares
- La tecnología que estemos usando: léase el framework que estemos usando. Las capacidades que nos brinde y sus limitaciones. Por ejemplo, Arena no les permite acceder a los controles propiamente dichos (ejemplo, acceder al texto actual de un TextBox, o a su propiedad `borderWith`).
- Los requerimientos de usuario:
  - La interfaz de usuario es un elemento más de nuestro sistema, y como todo el usuario/cliente puede tener una idea preconcebida de lo que quiere de ella. De hecho, la interfaz es uno de los puntos centrales ya que es justamente la cara visible para el usuario.

Así como hemos visto previamente cómo vincular nuestro dominio con la interfaz de usuario (a través de bindings, por citar un ejemplo), la manera de implementar una **tarea**  y su **navegación** va a depender de todos estos factores que nombramos.

# Un ejemplo concreto

Si estamos en una aplicación de facturación minorista de cualquier negocio, pensemos la vista del caso de uso "Crear factura", y en particular cómo sería la navegación para seleccionar al cliente. Tenemos varias opciones:

- el cliente se selecciona en un campo de autocompletado. El usuario no necesita salir de la página.
- el cliente se selecciona navegando una búsqueda por diferentes criterios, lo que implica ir hacia una vista nueva (una ventana de diálogo).
- adicionalmente, ¿qué estrategia adoptamos si se trata de un cliente nuevo? ¿necesitamos salir de la vista "Crear factura" y navegar a la vista del caso de uso "Crear cliente" o podemos cargar datos mínimos del cliente sin tener que perder el foco en lo que venimos haciendo? ¿la vista que carga el cliente está como una ventana de diálogo o está embebida en el caso de uso actual?

# Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
