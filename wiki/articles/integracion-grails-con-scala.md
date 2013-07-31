Pre-requisitos
--------------

-   Tenés que haber instalado el framework Grails en tu máquina, si no lo hiciste revisá [este link](instalacion-de-entorno-web-grails.html)

Instalación del plugin de Scala
-------------------------------

En el Dashboard, solapa Extensions, buscá "Scala IDE for Eclipse".

Integración propiamente dicha
-----------------------------

De todas maneras al momento de escribir este artículo no se integran del todo bien los compiladores groovy y scala. Entonces recomendamos separar tu aplicación en dos proyectos

-   **objetos de dominio en un proyecto en Scala**, que debe definir un group id, artifact id y version en un pom.xml para luego hacer un maven install. También se pueden utilizar herramientas similares como SBT o Gradle.
-   **un proyecto grails** que defina objetos vista, controllers, homes, etc.

Para que compile el proyecto grails, hay que apuntar al .jar generado del proyecto que contiene los objetos de dominio. Y para instalarlo en el web server, hay que modificar el BuildConfig.groovy, apuntando al groupId, artifactId y version del proyecto que contiene los objetos de dominio. También hay que agregar el compilador scala para hacer el build del .war.

**Ejemplo:**

`dependencies {`
`   // specify dependencies here under either 'build', 'compile', 'runtime', 'test' or 'provided' scopes e.g.`
`   def scalaVersion = '2.10.2'`
`   build "org.scala-lang:scala-compiler:$scalaVersion",`
`         "org.scala-lang:scala-library:$scalaVersion"`
`   build "org.uqbar-project:clientesTarjetaCredito-domain-scala:1.0.0-SNAPSHOT"`
`   ...`
