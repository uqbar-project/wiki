---
layout: article
title: Temario de Algoritmos III
---

El sitio oficial de la materia es <http://algo3.uqbar-project.org>

# Unidad 1 - Introducción a las UI

- [Definiciones Iniciales sobre interfaces de usuario](ui-definiciones-iniciales.html)
- [Clasificación](ui-clasificacion.html)
- [Integración de la UI en una arquitectura de un sistema de software](ui-arquitectura-general.html)
- [Elementos a tener en cuenta al programar una UI](ui-elementos-a-tener-en-cuenta-al-programar-ui.html)

# Unidad 2 - Cliente pesado. MVC.

## Contenidos principales

En esta unidad se verán los conceptos principales que permiten organizar el diseño de una interfaz de usuario. Estos conceptos pueden resumirse en:

- [Introducción a UI: componentes, organización espacial de la vista (layout), patrón MVC, binding](ui-mvcpesado-intro-mvc.html)
- [Navegación. Relación entre la navegación y los casos de uso del sistema.](ui-mvcpesado-navegacion.html)
- [Taller inicial de Arena (hands-on interactivo)](https://docs.google.com/document/d/17EvP3IGEbdzhC-da-V2iV3OB6yU4qYXbMNbycu3maPo/edit?usp=sharing), a partir de dos ejemplos se cuenta cómo se implementa el layout, algunos componentes visuales y el binding bidireccional, además del esquema MVC.
- [Application model. Extendiendo el MVC](ui-mvcpesado-mmvc.html)
- [Validaciones y manejo de errores en la UI](ui-mvcpesado-validaciones-errores.html)

## Material de lectura complementario

- Notas sobre MVC. El patrón MVC tiene multiples interpretaciones, de ellas la que nos parece más interesante es la que contempla el concepto de Application Model, también llamado [MMVC](http://c2.com/cgi/wiki?ModelModelViewController), [Presentation Model](https://martinfowler.com/eaaDev/PresentationModel.html) de Martin Fowler, MVVM o MVB (Model-View-Binder). Otras lecturas recomendadas son:
  - Discusión sobre las [múltiples interpretaciones y variantes](http://c2.com/cgi/wiki?ModelViewController) del patrón.
  - [Historia del patrón MVC](http://c2.com/cgi/wiki?ModelViewControllerHistory)
  - Finalmente una discusión sobre el elemento más controversial del patrón: [el controller](http://c2.com/cgi/wiki?WhatsaControllerAnyway).
- [Binding: Vinculación entre la vista y el modelo]([Formas de vincular una vista con el modelo de dominio](ui-mvcpesado-formas-de-vincular-una-vista-con-el-modelo-de-dominio.html)
)

## Entorno
  - Guía de Componentes
  - Bindings y demás controllers. Binding avanzado
  - Layouts
  - Navegación y manejo del estado
  - [Manejo de Transacciones](ui-mvcpesado-transaccion.html)
  - Qué pasa cuando no tenemos binding

# Unidad 3 - Web Estático

## Entorno

Solo necesitás un editor de texto (nosotros te recomendamos Visual Studio Code) y un navegador (el que vos prefieras)

## Artículos

- [Introducción a la arquitectura web](ui-web-intro-arquitectura.html)
- [HTML](html.html)
- [CSS](css.html)
- [Taller de maquetado web](https://docs.google.com/document/d/1UoEb9bzut-nMmB6wxDUVND3V8EymNFgOsw7Hka6EEkc/edit#)
- [Introducción a SASS](https://docs.google.com/document/d/1FX4pum1j4uq6s5nHhJRKitt7sYbLtKyBMmsifYkvGxc/edit?usp=sharing)
- [Responsiveness](responsiveness.html)

## Material de Internet

### HTML

- [Guía de tags HTML](https://www.w3.org/MarkUp/Guide/)
- [Referencia rápida de HTML](https://www.w3schools.com/html/default.asp)
- [Tutorial de HTML](https://www.htmlquick.com/es/tutorials.html), en español, para aprender lo básico

### Estilos

- [Página principal del W3C CSS](http://www.w3.org/Style/CSS/)
- [Tutorial de Estilos (CSS)](http://www.w3.org/Style/CSS/learning)
- [Tutorial layouts con CSS](http://www.subcide.com/articles/creating-a-css-layout-from-scratch/P2/)
- [Flexbox Froggy](https://flexboxfroggy.com/#es), un tutorial para aprender css y en particular el layout `display: flexbox`
- [Referencia completa de CSS](http://www.w3schools.com/css/default.asp)
- [Sass](https://sass-lang.com/)
- [Less](http://lesscss.org/), [Tutorial de Less](https://verekia.com/less-css/dont-read-less-css-tutorial-highly-addictive/)

## Ejemplos

- [Formulario web](https://github.com/uqbar-project/eg-formulario-web)
- [Hombre lobo - Bootstrap Twitter](https://github.com/uqbar-project/eg-hombrelobo-web)
- [Lista de amigos - taller CSS / Bootstrap Twitter](https://github.com/uqbar-project/eg-amigos-web)
- [Pantalla de Login - SASS](https://github.com/uqbar-project/eg-login-web)

# Unidad 4 - Web Client Side MVC.

## Entorno

- [Instalación de Entorno Angular](angular-instalacion.html)
- [Manejo de dependencias con NPM](npm-dependencias.html)

## Artículos 

- [Diapositivas Angular](https://docs.google.com/presentation/d/1vWYZMAuiGz2FwKA83lD9E-JUia2Sh6EQjbZqrBU6GAw/edit#slide=id.gaeb93a84c_0_95)

# Unidad 5 - Web Client Side FRP.

## Entorno

- [Instalación del entorno React](react-instalacion.html)

## Material

- [Apunte introductorio de React](https://docs.google.com/document/d/1Ez9eHep73VtCH7EMU3e8Hks97cGG2LbTwDjtrdgwrVk/edit)
- [Documentación oficial](https://reactjs.org/docs/hello-world.html)

## Ejemplos guía

Te dejamos para que te bajes una serie de ejemplos que sirven como guía para introducir los principales conceptos de Angular:

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

## Material

- [Introducción al desarrollo con Android](android-introduccion.html)
- [Cambiando el ícono / título de la app](android-cambiar-icono.html)
- [Ciclo de vida de las actividades](android-ciclo-de-vida.html)

## Ejemplos

### Kotlin

- [Conversor básico de millas a kilómetros](https://github.com/uqbar-project/eg-conversor-android-studio): primera activity y binding manual
  - [versión con Data Binding](https://github.com/uqbar-project/eg-conversor-binding-kotlin)
- [Listado de películas básico](https://github.com/uqbar-project/eg-peliculas-android-kotlin)
  - y [la versión REST](https://github.com/uqbar-project/eg-peliculas-android-kotlin/tree/rest) (hay otro branch para ver cómo se trabajaba en REST hasta la SDK 26 Oreo)
- [Primer ejemplo básico de un servicio REST](https://github.com/uqbar-project/eg-hola-mundo-android-kotlin)
- [Préstamos de libros (y de cosas)](https://github.com/uqbar-project/eg-prestamos-android-kotlin) en base a los contactos del celular

### Java

Podés hacer [esta búsqueda](https://github.com/uqbar-project?utf8=%E2%9C%93&q=android&type=&language=java) en los repositorios de la organización Uqbar

### Ionic

- [Carga de Productos](https://github.com/algo3-unsam/eg-productos-ionic): se integra con el reconocedor de código de barras del celular.
- [TODO List](https://github.com/uqbar-project/eg-todolist-ui-ionic) o lista de cosas para hacer, versión histórica en Ionic 1

### Ejemplos de Internet

- [Ejemplos de la página Developer Android](https://developer.android.com/samples)
- [Catálogo de aplicaciones Android de Java2S](http://www.java2s.com/Code/Android/CatalogAndroid.htm)
- [Ejemplos de Google](https://code.google.com/archive/p/apps-for-android/)
- [Repositorios con ejemplos en Googlesource de Android](https://android.googlesource.com/)

# Unidad 7 - Cierre y temas BONUS

## Material histórico

- [Wicket](como-bajar-y-correr-un-ejemplo-en-wicket.html) [(resumen pros y contras)](resumen-de-wicket--pros-y-contras.html)
