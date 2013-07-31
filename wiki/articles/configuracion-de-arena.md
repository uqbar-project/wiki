Qué debe tener un pom.xml de Arena
----------------------------------

Configurar el parent pom de la siguiente manera:

<parent>
`     `<groupId>`uqbar`</groupId>
`     `<artifactId>`arena-parent`</artifactId>
`     `<version>`1.3-SNAPSHOT`</version>
</parent>

Agregar dos referencias en el pom

-   al framework Arena
-   al proyecto de dominio (asumimos que vamos a tener dos proyectos separados, uno para el dominio y otro para la ui).

Por ejemplo, las dependencias en nuestro pom podrían quedar así:

`   `<dependencies>
`       `<dependency>
`           `<groupId>`uqbar-project.org`</groupId>
`           `<artifactId>`videoclub.domain`</artifactId>
`           `<version>`1.0-SNAPSHOT`</version>
`       `</dependency>
`       `<dependency>
`           `<groupId>`uqbar-project.org`</groupId>
`           `<artifactId>`arena`</artifactId>
`           `<version>`${arena-version}`</version>
`       `</dependency>
`       `<dependency>
`           `<groupId>`uqbar-project.org`</groupId>
`           `<artifactId>`arena-jface`</artifactId>
`           `<version>`${arena-version}`</version>
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
