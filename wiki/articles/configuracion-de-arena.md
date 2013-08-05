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

Integración con Scala
---------------------

Integración con Xtend
---------------------

Integración con Groovy
----------------------

Para bajarte los ejemplos, te recomendamos:

-   hacer checkout desde el SVN
-   una vez bajado el proyecto en tu workspace, botón derecho sobre el proyecto: Configure &gt; Convert to Maven project
-   Luego, botón derecho sobre el proyecto: Configure &gt; Convert to Groovy project
-   Y luego correr mvn compile o mvn install (Run As &gt; Maven install o bien crear una configuración de ejecución con el goal: "compile")

#### Crear un proyecto de Groovy con Arena

Necesitás agregar a tu pom las siguientes cosas:

-   Proyecto padre: arena-parent

<parent>
`   `<groupId>`uqbar`</groupId>
`   `<artifactId>`arena-parent`</artifactId>
`   `<version>`1.3-SNAPSHOT`</version>
</parent>

-   Dependencias: el are arena-jface, junit para los tests unitarios y tenés el plugin para groovy

<dependencies>
`   `<dependency>
`       `<groupId>`org.codehaus.mojo`</groupId>
`       `<artifactId>`build-helper-maven-plugin`</artifactId>
`       `<version>`1.8`</version>
`   `</dependency>
`   `<dependency>
`       `<groupId>`uqbar`</groupId>
`       `<artifactId>`arena-jface`</artifactId>
`       `<version>`${arena-version}`</version>
`   `</dependency>
`   `<dependency>
`       `<groupId>`junit`</groupId>
`       `<artifactId>`junit`</artifactId>
`       `<version>`3.8.1`</version>
`       `<scope>`test`</scope>
`   `</dependency>
</dependencies>

-   Y tenemos que definir el directorio donde están los fuentes como src/main/groovy en lugar de src/main/java que es el default para Maven:

<build>
`   `<sourceDirectory>`src/main/groovy`</sourceDirectory>
`   `<testSourceDirectory>`src/test/groovy`</testSourceDirectory>
`...`

Podés utilizar alguno de nuestros pom.xml como ejemplo.
