---
layout: article
title: Intro a MVC
categories: [mvc, cliente pesado, introduccion]
featured: true
---

# Qué necesitamos para construir una UI

Vamos a armar una lista de cosas que necesitamos para poder desarrollar una aplicación que tenga interfaz de usuario. Sobre algunas (en negrita) vamos a hablar en este momento, las otras irán apareciendo próximamente:

- Elementos gráficos / controles / widgets.
- Layout o forma de organizar visualmente los widgets.
- Binding (de atributos y de acciones).
- Navegación (cómo es el flujo de una vista a otra y cómo se pasa información desde una vista a otra).
- Manejo del estado conversacional de los datos que se cargan en la vista.

# Elementos gráficos o widgets (los controles)

Podemos clasificar a los controles de la siguiente manera:

- container: agrupan otros controles, siguiendo el pattern Composite. Son el formulario, el frame, el groupbox, las solapas, etc. 
- value holders o los que permiten ingresar un valor: calendars, textbox, checkbox, combo,- radiobutton, etc. 
- que trabajan sobre conjunto de datos: la grilla, el listbox, treeview (para mostrar - información jerárquica), etc.
- que disparan acciones: button, link  

# Layout

Cómo disponer esos elementos gráficos en la pantalla: 

## **layout definido en forma visual**

Eso suele generar código por abajo que está pensado en píxeles x@y (un punto) sobre la pantalla. Tener herramientas visuales WYSIWYG nos ayuda a no tener que explicar "quiero el botón acá", armar la pantalla con una toolbar de controles es más fácil que tener que explicarlo programando, resulta más intuitivo. Por otra parte lo visual me ata, la mayoría de las decisiones que tomo al construir la pantalla no quedan registradas en ningún lado. Yo se que estoy alineando los controles pero no digo en ningún lado alineado a qué porcentaje: ¿es un 40% del ancho de la pantalla? Y... "más o menos". Si tengo que agregar un campo nuevo a una pantalla con una herramienta visual tengo que volver a acomodar toda la pantalla, tirar para "abajo" los controles que ya están, volver a respetar el margen, etc. etc.

De la misma manera cada vez que me siento a diseñar una pantalla nueva tengo que tener en cuenta todas estas definiciones que no están escritas (o pueden estar en un documento que yo me tengo que encargar de respetar: estilos de fuente, disposición de controles, orden de la botonera: Cancelar primero y Aceptar después, o al revés, etc.) Esto ocurre aun cuando las herramientas visuales me generen código en Java (o bien generen código propietario que luego se interpreta para armar la parte visual), porque el proceso de creación de la pantalla queda en la cabeza del que la desarrolla... 

## ** layout definido en forma programática**

Cuando en lugar de tener una herramienta visual lo codifico me puede pasar lo mismo si no encuentro formas de decir:

- "los labels siempre se alinean a la derecha",
- "todos los controles de carga de datos del formulario ocupan 70% de la pantalla", 
- "la botonera se arma siempre: Cancelar, Aceptar y algunos botones configurables", etc. 

¿Qué buscamos al definir el layout de la interfaz? 1) que yo no hable en absoluto y trabaje sobre ideas relativas. 2) pero además, yo tengo que poder decir de alguna manera y en un solo lugar "esto va abajo" , "esto va arriba", "el botón Cancelar va a la izquierda del Aceptar", "todos los botones se agrupan abajo del formulario y centrado", de esa manera es más fácil después alinear las botoneras de todas las pantallas a la izquierda, o bien cambiar el layout de la botonera (por ejemplo para que los botones aparezcan a la derecha de la pantalla uno abajo del otro).

![image](/img/wiki/thinking_layout.jpg)

Lo que está dentro de la nube debería "reificarse" (llevar esa abstracción al código, esa abstracción es el Layout).

# Binding

Lo que hace que la pantalla tenga sentido es que los elementos gráficos estén relacionados con el dominio. O sea, hay una relación entre los elementos gráficos y los de dominio: muchos elementos gráficos son representaciones visuales de elementos de mi dominio,

- que pueden mostrar información o permitir editarla (la fecha de un alquiler, el nombre de un actor),
- o disparar acciones (el alta de un socio, la búsqueda de películas según un criterio, etc.)

# Intro a MVC

El binding relaciona parte visual y la información que está relacionada. A la primera se la suele llamar vista, a la segunda modelo.

El binding se puede dar de dos maneras:

- la vista es la única que actualiza el modelo V –> M. Ejemplo: un procesador de texto. Entonces es claro que el flujo va solamente de pantalla a modelo.
- la vista actualiza el modelo o el modelo puede actualizarse desde otro contexto y tonces el modelo dispara la notificación a la vista. V <--> M. La segunda visión (la notificación en ambos sentidos entre modelo y vista) es la que da sentido a separar modelo de vista para poder reutilizar el modelo en más de un contexto (ej: si queremos tener una pantalla de Alta de un Socio para un videoclub o bien subir un Excel con un conjunto de socios y que dicha carga sea masiva)
  
Vamos a darle la responsabilidad a alguien (que llamaremos controller) para que se encargue de manejar el binding de atributos y acciones en forma bidireccional (o sea: hacia la vista y hacia el dominio). Para eso vamos a trabajar de la siguiente manera: la vista se va a registrar como interesada en el modelo. Entonces cada vez que alguien actualice el modelo, se va a disparar una notificación hacia determinados interesados (esta sería una implementación del Observer pattern).

![mvc](/img/wiki/mvc.jpg)

En definitiva, si la aplicación está bien construida lo que pasa es:

1. un cambio en la vista dispara un mensaje a un objeto de negocio
2. el negocio modifica su estado interno y dispara la notificación a la vista (en realidad, a todos los interesados, la vista es sólo uno más)
3. la vista se actualiza con la información del objeto de negocio (esto puede significar que ahora un campo esté deshabilitado o que mostremos el socio en color azul porque ya no debe plata)

O sea: vista y negocio están sincronizados todo el tiempo, cada uno sabe qué tiene que hacer:

- el negocio sabe su lógica
- la vista sabe cómo mostrar la información del negocio.

# Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
