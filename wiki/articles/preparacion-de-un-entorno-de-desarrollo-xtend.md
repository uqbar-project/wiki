![](Xtend-logo.png "Xtend-logo.png")

### Download e instalación base

La página oficial de downloads de XTend es [ésta](http://www.eclipse.org/xtend/download.html). Ahí vas a ver dos opciones:

-   La fácil: **Full Eclipse** (bajarte el Eclipse + el plugin directamente)
-   [Configurar un Eclipse Básico](http://uqbar-wiki.org/index.php?title=Preparacion_de_un_entorno_de_desarrollo_Java) y luego descargar el plugin
    -   a través del Marketplace haciendo Help &gt; Eclipse Marketplace y buscando por palabra clave "Xtend" (aparece como "Eclipse Xtend x.x.x")
    -   o haciendo Help &gt; Install New Software ... y seleccionar la URL <http://download.eclipse.org/modeling/tmf/xtext/updates/composite/releases/> (le podés dar el nombre que quieras al update site)

El plugin de xtend es relativamente grande. Si tu instalación de eclipse tiene además otros plugins para programar en otros lenguajes, puede que sea recomendable armar una instalación aparte. Si uno usa muchos lenguajes con eclipse como IDE, puede ser recomendable tener un eclipse para cada uno (salvo obviamente en el caso en que varios lenguajes se utilicen en el mismo proyecto). Eso hace que el IDE sea más liviano y minimiza posibles problemas de incompatibilidad entre los diferentes features.

De todos los items que aparecerán en el update site debemos elegir "xtend". Si vas a usar maven te conviene instalar la integración con m2eclipse (pero antes instalar el m2eclipse).

Acá un screenshot a modo de ejemplo.

**Nota:** reemplazar 'XXX' por la última versión estable del plugin.

![](Xtend-installation.png "Xtend-installation.png")

Deben utilizar la versión 2.7.3 de Xtend (o posterior)

### Configuraciones default del eclipse

Antes que nada chequeá las [Configuraciones generales para cualquier Eclipse](configuraciones-generales-para-cualquier-eclipse.html)

### ¿Cómo empezar?

-   Crear un proyecto Java.
-   Luego crear una clase XTend.
-   El eclipse mostrará un error similar a

La solución a ese problema se explica en la siguiente sección. Para más detalles pueden mirar <http://www.eclipse.org/xtend/download.html>

### Trabajo con Maven

Para poder utilizar Maven con Xtend tenés que instalarlo como se sugiere [aquí](http://uqbar-wiki.org/index.php?title=Gu%C3%ADa_de_Instalaci%C3%B3n_de_Maven)

### Configuración de la librería de xtend

Para que compile el código xtend dentro de un proyecto hace falta tener una librería (en cada proyecto). La "famosa" 'org.eclipse.xtext.xbase.lib'.

Acá hay dos opciones, dependiendo de cómo estés manejando las dependencias en tu proyecto.

-   Si no estás usando maven, podés simplemente ir a las propiedades del proyecto, "Java Build Path", luego en la solapa "Libraries", "Add Library", y seleccionar "Xtend Library".

<!-- -->

-   Si vas a usar MAVEN (opción que recomendamos), no deberías hacer el paso anterior, porque eso va a hacer que las cosas compilen en eclipse (momentáneamente), pero no le estamos indicando a maven que el proyecto usa la librería de xtend, con lo cual nos va a traer problemas a futuro (por ejemplo al correr el proyecto si es una webapp va a tirar error por no encontrar las clases de xtend). En ese caso lo más fácil es que heredes de un pom de uqbar que ya hace el laburo por vos (ya declara las dependencias)

`   `<parent>
`       `<groupId>`org.uqbar-project`</groupId>
`       `<artifactId>`uqbar-xtend-parent`</artifactId>
`       `<version>`2.7.3`</version>
`   `</parent>

Luego boton derecho, "Maven" "Update Project..."

### Tips

-   Para que cuando hagas New &gt; File te aparezcan las clases y las interfaces Xtend, Window &gt; Customize Perspective... &gt; solapa Menu Visibility &gt; expandís File | New &gt; y seleccionás las de xtend (Xtend class, inteface, annotation y enum).

### Documentación

-   [Documentación oficial](http://www.eclipse.org/xtend/documentation.html)

### Links útiles

-   Si venís del mundo Java chequeá [este link](http://jnario.org/org/jnario/jnario/documentation/20FactsAboutXtendSpec.html)

