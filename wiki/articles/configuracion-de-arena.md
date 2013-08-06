Pre-requisitos
--------------

Asumimos que tenés instaladas las herramientas básicas según se explica en [este instructivo](preparacion-de-un-entorno-de-desarrollo-java.html), incluyendo Maven.

Qué debe tener un pom.xml de Arena
----------------------------------

Configurar el parent pom de la siguiente manera:

<parent>
`     `<groupId>`uqbar`</groupId>
`     `<artifactId>`arena-parent`</artifactId>
`     `<version>`1.3-SNAPSHOT`</version>
</parent>

Agregar dos referencias en el pom

-   al framework Arena (consideramos Arena = Arena-JFace como predeterminado)
-   al proyecto de dominio (asumimos que vamos a tener dos proyectos separados, uno para el dominio y otro para la ui).
-   a otros frameworks como JUnit

Por ejemplo, las dependencias en nuestro pom podrían quedar así:

`   `<dependencies>
`       `<dependency>
`           `<groupId>`uqbar`</groupId>
`           `<artifactId>`arena-jface`</artifactId>
`           `<version>`${arena-version}`</version>
`       `</dependency>
`       `<dependency>
`           `<groupId>`uqbar-project.org`</groupId>
`           `<artifactId>`videoclub.domain`</artifactId>
`           `<version>`1.0-SNAPSHOT`</version>
`       `</dependency>
`       `<dependency>
`           `<groupId>`junit`</groupId>
`           `<artifactId>`junit`</artifactId>
`           `<version>`4.11`</version>
`           `<scope>`test`</scope>
`       `</dependency>
`   `</dependencies>

Si no querés tocar el pom.xml a mano, podés agregarlo a través del plugin M2clipse: botón derecho sobre el proyecto, Maven &gt; Add Dependency &gt; buscás "arena" y tiene que aparecer "uqbar arena", buscás la versión que querés (o si tenés dudas la última) y aceptás. Entonces el plugin va a descargarlo (si no lo tiene en tu repositorio local). Lo mismo con las demás dependencias que necesites.

### Importante, para correr cualquier aplicación de Arena

En Run &gt; Run Configurations... &gt; Java Application &gt; New launch configuration (buscá el botón de la toolbar que está a la izquierda) y en la solapa Arguments, tenés que indicarle en VM Arguments que use el Launcher propio de Arena:

`-Djava.system.class.loader=com.uqbar.apo.APOClassLoader`

de lo contrario te va a aparecer un mensaje de error:

`Exception in thread "main" java.lang.RuntimeException: Esta aplicación no está corriendo con el ClassLoader necesario. Corra  la aplicación con el siguiente parámetro para la VM: -Djava.system.class.loader=com.uqbar.apo.APOClassLoader. El ClassLoader actual es: sun.misc.Launcher$AppClassLoader@6fd3633c`
`   at org.uqbar.arena.Application.validateClassLoader(Application.java:32)`
`   at org.uqbar.arena.Application.`<init>`(Application.java:24)`

En muchos ejemplos tenemos un archivo .launch que tiene esta configuración ya cargada.

Integración con Scala
---------------------

Asumimos que además del entorno básico ya te instalaste Scala según [este instructivo](preparacion-de-un-entorno-de-desarrollo-scala.html).

Para bajarte los ejemplos, te recomendamos:

-   hacer checkout desde el SVN
-   una vez bajado el proyecto en tu workspace, botón derecho sobre el proyecto: Configure &gt; Convert to Maven project
-   Luego, botón derecho sobre el proyecto: Configure &gt; Convert to Scala project
-   Y luego correr mvn compile o mvn install (Run As &gt; Maven install o bien crear una configuración de ejecución con el goal: "compile")

#### Crear un proyecto de Arena en Scala

TODO

Integración con Xtend
---------------------

Asumimos que además del entorno básico ya te instalaste Xtend según [este instructivo](preparacion-de-un-entorno-de-desarrollo-xtend.html).

Para bajarte los ejemplos, te recomendamos:

-   hacer checkout desde el SVN
-   una vez bajado el proyecto en tu workspace, botón derecho sobre el proyecto: Configure &gt; Convert to Maven project
-   Luego, botón derecho sobre el proyecto &gt; Configure &gt; Add Text Nature
-   Luego correr mvn compile o mvn install (Run As &gt; Maven install o bien crear una configuración de ejecución con el goal: "compile")
-   Y finalmente botón derecho sobre el proyecto &gt; Build Path &gt; Add Library &gt; Xtend Library

#### Crear un proyecto de Arena en Xtend

Al pom.xml se le puede agregar una dependencia:

<dependency>` `
`     `<groupId>`org.eclipse.xtend`</groupId>` `
`     `<artifactId>`org.eclipse.xtend.standalone`</artifactId>
`     `<version>`2.4.2`</version>
</dependency>

Pero no siempre es feliz el plugin de maven, así que si hay inconvenientes, comentar la dependencia y agregar la libería Xtend a mano en el build path del proyecto.

Integración con Groovy
----------------------

Asumimos que además del entorno básico ya te instalaste Groovy según [este instructivo](http://uqbar-wiki.org/index.php?title=Preparacion_de_un_entorno_de_desarrollo_Groovy).

Para bajarte los ejemplos, te recomendamos:

-   hacer checkout desde el SVN
-   una vez bajado el proyecto en tu workspace, botón derecho sobre el proyecto: Configure &gt; Convert to Maven project
-   Luego, botón derecho sobre el proyecto: Configure &gt; Convert to Groovy project
-   Y luego correr mvn compile o mvn install (Run As &gt; Maven install o bien crear una configuración de ejecución con el goal: "compile")

#### Crear un proyecto de Arena en Groovy

A las configuraciones generales del pom.xml (referenciar al parent-project y las dependencias de arena + junit), hay que incorporar:

-   Dependencias: el plugin para groovy

<dependencies>
`   `<dependency>
`       `<groupId>`org.codehaus.mojo`</groupId>
`       `<artifactId>`build-helper-maven-plugin`</artifactId>
`       `<version>`1.8`</version>
`   `</dependency>
`   `<dependency>
`       `<groupId>`uqbar`</groupId>
`       `<artifactId>`arena-jface`</artifactId>
`       ...`
</dependencies>

-   Y definir el directorio donde están los fuentes como src/main/groovy en lugar de src/main/java que es el default para Maven:

<build>
`   `<sourceDirectory>`src/main/groovy`</sourceDirectory>
`   `<testSourceDirectory>`src/test/groovy`</testSourceDirectory>
`...`

Podés utilizar alguno de nuestros pom.xml como ejemplo.
