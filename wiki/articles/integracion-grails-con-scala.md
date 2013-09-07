Pre-requisitos
--------------

-   Tenés que haber instalado el framework Grails en tu máquina, si no lo hiciste revisá [este link](instalacion-de-entorno-web-grails.html)

Instalación del plugin de Scala
-------------------------------

En el Dashboard, solapa Extensions, buscá "Scala IDE for Eclipse".

Integración propiamente dicha
-----------------------------

Al momento de escribir este artículo no se integran del todo bien los compiladores groovy y scala. Entonces recomendamos separar tu aplicación en dos proyectos

-   **objetos de dominio en un proyecto en Scala**, que debe definir un group id, artifact id y version en un pom.xml para luego hacer un maven install. También se pueden utilizar herramientas similares como SBT o Gradle.
-   **un proyecto grails** que defina objetos vista, controllers, homes, etc.

**Consejos:**

1.  Para que compile el proyecto grails, hay que apuntar al .jar generado del proyecto que contiene los objetos de dominio.
2.  Y para que corra la aplicación en el web server, hay que modificar el BuildConfig.groovy, apuntando al groupId, artifactId y version del proyecto que contiene los objetos de dominio.
3.  También hay que agregar el compilador scala para hacer el build del .war.

**Ejemplo:**

`dependencies {`
`   // specify dependencies here under either 'build', 'compile', 'runtime', 'test' or 'provided' scopes e.g.`
`   def scalaVersion = '2.10.2'`
`   build "org.scala-lang:scala-compiler:$scalaVersion", (3)`
`         "org.scala-lang:scala-library:$scalaVersion" (3)`
`   build "org.uqbar-project:clientesTarjetaCredito-domain-scala:1.0.0-SNAPSHOT" (2)`
`   ...`
`plugins {`
`   ...      `
`   build ":scala:0.6.4" (3)`
`   ...`

Troubleshooting
---------------

Si al correr la aplicación aparece un error "missing src/scala", o "BuildException: Attribute 'srcdir' is not set.", la solución es generar un source folder nuevo que sea: "src/scala", que quede vacío. Luego volver a ejecutar run-app.
