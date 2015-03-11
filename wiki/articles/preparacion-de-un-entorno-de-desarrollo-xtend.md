![](Xtend-logo.png "Xtend-logo.png")

### Download e instalación base

-   Ingresar a <https://www.eclipse.org/modeling/tmf/downloads/>

<!-- -->

-   Buscar la versión **2.7.3** y descargar el link "All-In-One p2 Repo Update Site All-In-One Update Site". Elegir cualquier mirror y descargarlo en cualquier directorio en el que se tengan permisos

<!-- -->

-   Iniciar el Eclipse, hacer Help &gt; Install New Software ... botón Add, Archive y seleccionar el archivo que se descargaron

<!-- -->

-   Seleccionar el check Xtend IDE, hacer click en Next y luego en Finish

![](Xtend-installation.png "Xtend-installation.png")

-   Reiniciar el Eclipse

**Nota Importante:** Deben utilizar la versión *2.7.3* de Xtend (versiones anteriores a 2.7 ó posteriores tienen problemas de compatibilidad para utilizar los ejemplos)

### Configuraciones default del eclipse

Antes que nada chequeá las [Configuraciones generales para cualquier Eclipse](configuraciones-generales-para-cualquier-eclipse.html)

### ¿Cómo empezar?

-   Crear un proyecto Java.
-   Luego crear una clase XTend.
-   El eclipse mostrará un error similar a `Mandatory` `library` `bundle` `'org.eclipse.xtext.xbase.lib'` `2.3.0` `or` `higher` `not` `found` `on` `the` `classpath."`

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

