---
layout: article
title: Temario de Algoritmos III
---

El sitio oficial de la materia es <http://algo3.uqbar-project.org>

# Unidad 1 - Introducción a las UI

- [Definiciones Iniciales sobre interfaces de usuario](ui-definiciones-iniciales.html)
- [Clasificación](ui-clasificacion.html)
- [Integración de la UI en una arquitectura de un sistema de software](ui-arquitectura-general.html)
- [Elementos a tener en cuenta al programar una UI](elementos-a-tener-en-cuenta-al-programar-ui.html)

# Unidad 2 - Cliente pesado. MVC.

## Entorno

- [Instalación del entorno Arena](arena-instalacion.html)
- [Configuración de Arena](arena-configuracion.html)

## Artículos

- Introducción: qué necesitamos para construir una UI. Intro a MVC.
- [Página principal de Arena](https://sites.google.com/site/programacionui/herramientas/arena?authuser=0) donde se explican los conceptos principales, y podés descargarte una gran cantidad de ejemplos
- [Formas de vincular una vista con el modelo de dominio](ui-mvnpesado-formas-de-vincular-una-vista-con-el-modelo-de-dominio.html)
- [Validaciones y manejo de errores en la UI](validaciones-y-manejo-de-errores-en-la-ui.html)

# Unidad 3 - Web Básico

## Entorno

## Artículos

- [Descripción global de la arquitectura web](descripcion-global-de-la-arquitectura-web.html)
- [Responsiveness](responsiveness.html)
- [Referencia de HTML](html.html)
- [Referencia de CSS](css.html)

# Unidad 4 - Web Client Side MVC.

## Entorno

- [Instalación de Entorno Javascript](instalacion-de-entorno-javascript.html)
- [Instalación de Entorno Angular](instalacion-de-entorno-angular.html)

## Artículos 


# Unidad 5 - Web Client Side FRP.

## Entorno

- [Instalación del entorno React](react-instalacion.html)

## Material

- [Apunte introductorio de React](https://docs.google.com/document/d/1Ez9eHep73VtCH7EMU3e8Hks97cGG2LbTwDjtrdgwrVk/edit)
- [Documentación oficial](https://reactjs.org/docs/hello-world.html)

## Ejemplos guía

Te dejamos para que te bajes una serie de ejemplos que sirven como guía para introducir diferentes conceptos:

- [Hola-mundo](https://github.com/uqbar-project/eg-hola-mundo-react/): el primer ejemplo en React, contiene dos componentes
  - un contador al que podemos sumarle o restarle uno
  - un label que permite saludar a una persona (muestra cómo pasarle parámetros a un componente)
  - explica los conceptos **componente**, props, estado, y da una introducción al **testeo unitario de componentes con Jest + Enzyme**
- [Conversor](https://github.com/uqbar-project/eg-conversor-react): el tradicional conversor de millas a kilómetros.
  - Hay un branch [**conversor-inmutable**](https://github.com/uqbar-project/eg-conversor-react/tree/conversor-inmutable) que define un conversor con estado, más parecido a los ejemplos vistos anteriormente. Explica el ciclo de vida mutación de estado y posterior render, testing y la arquitectura de ambas soluciones
- [Tareas de un equipo de desarrollo](https://github.com/uqbar-project/eg-tareas-react) contra un backend REST (muestra el uso de servicios asincrónicos, sin Redux)
- Tenés un ejemplo básico del [contador utilizando **React Redux**](https://github.com/uqbar-project/eg-contador-react-redux), con React Bootstrap
  - Hay una versión mejorada de las funciones reductoras, [en este branch](https://github.com/uqbar-project/eg-contador-react-redux/tree/reducers)
- Mundial 2018 - Rusia: es una serie de ejemplos que permite ir conociendo diferentes conceptos de la tecnología
  - comenzamos con una [búsqueda de los países que juegan la copa](https://github.com/uqbar-project/eg-mundial2018-react), en base al grupo que le tocó en la primera fase o en la descripción. Repasamos **componentes customs, estado, props, servicios**, etc. 
  - luego agregamos la posibilidad de [ingresar los resultados de los partidos del mundial](https://github.com/uqbar-project/eg-mundial2018-react/tree/fase2), definiendo **rutas con React**
  - en la tercera versión podrás ver la [tabla de posiciones que se irá actualizando dinámicamente a medida que cargás los resultados](https://github.com/uqbar-project/eg-mundial2018-react/tree/fase3), utilizando **React Redux** para tener en un **store** los resultados actualizados del mundial
- Y por último tenés un tutorial paso a paso que desarrolla una aplicación en React, NodeJS y Firebase para mostrar [charlas de una conferencia](https://github.com/fdodino/conferencias-FSD)

## Ejemplos en Internet

- [React Rocks](https://react.rocks/)
- [Ejemplos de la página de React](https://reactjs.org/community/examples.html)
- [11 ejemplos - DevTo](https://dev.to/drminnaar/11-react-examples-2e6d)

# Unidad 6 - Aplicaciones móviles

## Entorno 

- [Preparacion de un entorno de desarrollo Android](android-instalacion.html)
- [Herramientas de desarrollo con Android](herramientas-de-desarrollo-con-android.html)

# Unidad 7 - Cierre y temas BONUS

## Material histórico

- [Grails](instalacion-de-entorno-web-grails.html)
- [Wicket](como-bajar-y-correr-un-ejemplo-en-wicket.html) [(resumen pros y contras)](resumen-de-wicket--pros-y-contras.html)
