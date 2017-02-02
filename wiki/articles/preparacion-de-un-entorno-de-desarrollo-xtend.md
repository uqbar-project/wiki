---
layout: article
title: Preparacion de un entorno de desarrollo xtend
---

![](Xtend-logo.png "Xtend-logo.png")

### Download e instalación base

En el 2016, todos los ejemplos de uqbar estarán utilizando la versión 2.9.1 que se puede descargar en <http://www.eclipse.org/xtend/download.html> (con plugins tanto para Eclipse como para IntelliJ IDEA).

Como es posible que el equipo de desarrollo de Xtend saque nuevas versiones durante el año, dejamos una alternativa para poder instalar la versión 2.9.1 cuando ya no sea la última versión. El lector se preguntará, ¿no podría yo utilizar el plugin de las versiones 2.9.2 ó 2.9.3? Es posible, pero ya hemos experimentado problemas de retrocompatibilidad graves que impedían ejecutar los ejemplos de uqbar correctamente. Entonces nuestra recomendación es utilizar 2.9.1 como versión de tu entorno de desarrollo xtend.

Para eso:

-   Ingresar a <https://www.eclipse.org/modeling/tmf/downloads/>

<!-- -->

-   Buscar la versión **2.9.1** y descargar el link "All-In-One Update Site". Elegir cualquier mirror y descargarlo en cualquier directorio en el que se tengan permisos

<!-- -->

-   Iniciar el Eclipse, hacer Help &gt; Install New Software ... botón Add, Archive y seleccionar el archivo que se descargaron

<!-- -->

-   Seleccionar el check Xtend IDE, hacer click en Next y luego en Finish

![](Xtend-installation.png "Xtend-installation.png")

-   Reiniciar el Eclipse

### Configuraciones default del eclipse

Antes que nada chequeá las [Configuraciones generales para cualquier Eclipse](configuraciones-generales-para-cualquier-eclipse.html)

### ¿Cómo empezar?

-   Crear un proyecto Maven (si no instalaste Maven hacelo como se sugiere [aquí](http://uqbar-wiki.org/index.php?title=Gu%C3%ADa_de_Instalaci%C3%B3n_de_Maven))
    -   en la primera ventana, clickear en la opción "Create a simple project (Skip archetype selection)", luego Next...
    -   definir un groupId, que puede ser el edu.xxxx (nombre de la materia). Ej: edu.algo2 | edu.dds
    -   definir un artifactId, que se asocia al nombre de tu proyecto

Para que compile el código xtend dentro de un proyecto hace falta tener una librería (en cada proyecto). La "famosa" 'org.eclipse.xtext.xbase.lib'. En ese caso lo más fácil es que heredes de un pom de uqbar que ya hace el laburo por vos (ya declara las dependencias)

`   `<parent>
`       `<groupId>`org.uqbar-project`</groupId>
`       `<artifactId>`uqbar-xtend-parent`</artifactId>
`       `<version>`2.9.1`</version>
`   `</parent>

Esto lo podés hacer en la misma ventana del wizard que crea el proyecto Maven o bien editando el pom.xml de tu proyecto Maven recientemente creado. Luego boton derecho, "Maven" "Update Project..."

¿Dónde van las clases xtend?

-   En src/main/java
-   En src/main/generated-sources vas a tener los archivos .java que se generan en base a los archivos de xtend. ¡No los toques! Porque cada cambio que hagas en tu clase xtend va a pisar los cambios de los archivos .java. En general no deberías mirar nunca el java que genera, porque además utiliza construcciones menos simples que si programaras directamente en java.

### Tips

-   Para que cuando hagas New &gt; File te aparezcan las clases y las interfaces Xtend, Window &gt; Customize Perspective... &gt; solapa Menu Visibility &gt; expandís File | New &gt; y seleccionás las de xtend (Xtend class, inteface, annotation y enum).

### Problema con el menu de Eclipse en Ubuntu 13.10

-   [Problema de Eclipse con Ubuntu 13.10](http://uqbar-wiki.org/index.php?title=Problema_de_Eclipse_con_Ubuntu_13.10)

### Documentación

-   [Documentación oficial](http://www.eclipse.org/xtend/documentation.html)

### Links útiles

-   Si venís del mundo Java chequeá [este link](http://jnario.org/org/jnario/jnario/documentation/20FactsAboutXtendSpec.html)

