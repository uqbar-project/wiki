Pre-requisitos
--------------

Asumimos que tenés instaladas las herramientas básicas según se explica en [este instructivo](preparacion-de-un-entorno-de-desarrollo-java.html), incluyendo Maven.

Qué debe tener un pom.xml de Arena
----------------------------------

Configurar el parent pom de la siguiente manera:

<parent>
`   `<groupId>`org.uqbar-project`</groupId>
`   `<artifactId>`uqbar-parent-project`</artifactId>
`   `<version>`1.7`</version>
</parent>

Salvo que estemos trabajando con xtend, en cuyo caso deberías usar éste:

<parent>
`    `<groupId>`org.uqbar-project`</groupId>
`    `<artifactId>`uqbar-xtend-parent`</artifactId>
`    `<version>`2.6.2`</version>
</parent>

### Dependencias para proyectos de dominio

Si vas a definir tus objetos de dominio en un proyecto aparte (cosa que recomendamos) tenés que definir esta dependencia

<dependency>
`   `<groupId>`org.uqbar-project`</groupId>
`   `<artifactId>`uqbar-domain`</artifactId>
`   `<version>`3.3`</version>
</dependency>

### Dependencias para proyectos de UI

Agregar dos referencias en el pom

-   al framework Arena (consideramos Arena = Arena-JFace como predeterminado)
-   al proyecto de dominio (asumimos que vamos a tener dos proyectos separados, uno para el dominio y otro para la ui).
-   a otros frameworks como JUnit

Por ejemplo, las dependencias en nuestro pom podrían quedar así:

`   `<dependencies>
`       `<dependency>
`           `<groupId>`org.uqbar-project`</groupId>
`           `<artifactId>`arena-jface`</artifactId>
`           `<version>`3.3-SNAPSHOT`</version>
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

Asumimos que además del entorno básico ya te instalaste Scala según el instructivo para [Preparacion de un entorno de desarrollo Scala](preparacion-de-un-entorno-de-desarrollo-scala.html).

Para bajarte los ejemplos, te recomendamos:

-   hacer checkout desde el SVN
-   una vez bajado el proyecto en tu workspace, botón derecho sobre el proyecto: Configure &gt; Convert to Maven project

### Crear un proyecto de Arena en Scala

Las instrucciones para tener un proyecto Scala que utilice Arena son las mismas que para cualquier [Proyecto Maven con Scala](preparacion-de-un-entorno-de-desarrollo-scala-creacion-de-un-proyecto-maven-con-scala.html)

En caso de duda podés utilizar alguno de nuestros pom.xml como ejemplo.

Integración con Xtend
---------------------

Asumimos que además del entorno básico ya te instalaste Xtend según [este instructivo](preparacion-de-un-entorno-de-desarrollo-xtend.html).

Para bajarte los ejemplos, te recomendamos:

-   hacer checkout desde el SVN
-   una vez bajado el proyecto en tu workspace, botón derecho sobre el proyecto: Configure &gt; Convert to Maven project
-   luego correr mvn compile o mvn install (Run As &gt; Maven install o bien crear una configuración de ejecución con el goal: "compile")

Con esto no debería ser necesario hacer ninguna otra cosa, en caso de que aparezca un error similar a éste:

` Couldn't find the mandatory library 'org.eclipse.xtext.xbase.lib' 2.4.0 or higher on the project's classpath.`

se puede salir del paso haciendo click derecho sobre el proyecto &gt; Build Path &gt; Add Library &gt; Xtend Library

#### Crear un proyecto de Arena en Xtend

Recordamos que deben trabajar con el parent-project [específico](configuracion-de-arena-qu-c3-a9-debe-tener-un-pom-xml-de-arena.html). Esto evita tener que agregar manualmente la dependencia. En caso de ser necesario, te dejamos un ejemplo posible para el pom.xml:

<dependency>` `
`     `<groupId>`org.eclipse.xtend`</groupId>` `
`     `<artifactId>`org.eclipse.xtend.standalone`</artifactId>
`     `<version>`2.6.2`</version>
</dependency>

Pero no siempre es feliz el plugin de maven, así que si hay inconvenientes, comentar la dependencia y agregar la libería Xtend a mano en el build path del proyecto.

Integración con Groovy
----------------------

Asumimos que además del entorno básico ya te instalaste Groovy según [este instructivo](preparacion-de-un-entorno-de-desarrollo-groovy.html).

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

Troubleshooting
---------------

¿Qué hacer cuando nos bajamos ejemplos (o desarrollamos uno nuevo) y no nos andan? Tenemos que revisar estas cosas

### Maven

Revisá que Maven esté correctamente instalado en tu máquina y que tenés el settings.xml correctamente configurado. Cualquier duda fijate en [el tutorial de instalación de Maven](preparacion-de-un-entorno-de-desarrollo-java-maven.html).

También asegurate que la versión de Maven sea 3.0.x o posterior, o vas a tener un mensaje de error similar a éste:

`[INFO] Unable to initialise extensions`
`Component descriptor role: 'com.jcraft.jsch.UIKeyboardInteractive', implementation: 'org.apache.maven.wagon.providers.ssh.jsch.interactive.PrompterUIKeyboardInteractive', role hint: 'default' has a hint, but there are other implementations that don't`

### Checkout desde el SVN

En general los ejemplos se bajan desde la estructura "proyecto/trunk", si los bajás desde el directorio proyecto, vas a tener algunos conflictos de directorio.

### Source folders del proyecto

Los source folders de los proyectos (que tienen maven como estructura central del proyecto) deben ser

-   para proyectos Java o Xtend: src/main/java y src/test/java
-   para proyectos Groovy: src/main/groovy y src/test/groovy
-   y para proyectos Scala es src/main/scala y src/test/scala.

Si te aparece como source folder sólo el src, o bien si no tenés source folders, una de dos: 1) corré el plugin de maven: botón derecho sobre el proyecto &gt; Configure &gt; Maven project (o mvn compile) 2) si eso no soluciona el problema, agregá los source folder a mano: botón derecho sobre el proyecto &gt; Build path &gt; new source folder o bien parado sobre el directorio src/main/el-que-corresponda botón derecho &gt; Build path &gt; Add as source folder

### Naturaleza del proyecto

Si estás en una tecnología distinta de Java, tenés que asegurarte que el proyecto tenga el "nature" de esa tecnología. Si no lo tiene, click derecho sobre el proyecto &gt; Configure

-   para Xtend, Add Xtext nature
-   para Groovy, Add Groovy nature
-   para Scala, Add Scala nature

### JDK

Revisá que tengas instalada una JDK (no JRE, tiene que ser JDK con las herramientas para desarrollar en Java como el compilador, debug, etc.) y que la versión de ese JDK sea 1.7 ó superior. Si querés usar una JDK 1.6 ó inferior te va a aparecer el siguiente mensaje de error

`java.lang.UnsupportedClassVersionError: ---aplicación de Arena--- : Unsupported major.minor version 51.0`

porque Arena está compilado con una JDK 1.7 (1.6 ya no tiene soporte de Sun)

### Problemas para encontrar la ventana ejecutable

Si te aparece un error similar a éste al correr un "launcher":

`Error: no se ha encontrado o cargado la clase principal org.uqbar.arena.examples.conversor.xtend.ConversorWindow`

Revisá

-   si tenés correctamente definidos los source folders (punto anterior)
-   si tenés las librerías de Xtend/Groovy/Scala importadas en tu proyecto (sólo la del lenguaje en el que te lo bajaste, claro está). En caso de que no sea así, las podés agregar manualmente: click derecho sobre el proyecto &gt; propiedades &gt; java build path &gt; solapa "libraries" &gt; "add library" y elegir la que corresponda
-   por último, si tenés apuntado en tu proyecto un JDK válido (que apunte a un directorio que exista en tu PC, revisá Window &gt; Preferences &gt; Installed JREs)

### Problemas al correr el ejecutable

Si te aparece un error como éste al correr un launcher:

`Error occurred during initialization of VM`
`java.lang.Error: java.lang.ClassNotFoundException: org.uqbar.arena.aop.ArenaClassLoader`
`   at java.lang.ClassLoader.initSystemClassLoader(Unknown Source)`
`   at java.lang.ClassLoader.getSystemClassLoader(Unknown Source)`
`Caused by: java.lang.ClassNotFoundException: org.uqbar.arena.aop.ArenaClassLoader`
`   at java.net.URLClassLoader$1.run(Unknown Source)`

El problema está en el launcher, el ArenaClassLoader está deprecado, se reemplazó por el APOClassLoader (más arriba te dice cómo configurarlo).

Otro problema que puede aparecer es:

`Error occurred during initialization of VM`
`java.lang.Error: java.lang.ClassNotFoundException: ....APOClassLoader`

entonces el problema es que no te descargó las dependencias de Maven correctamente. Revisá los directorios de tu usuario/.m2/repository porque seguramente te falte bajar dependencias, podés probar haciendo Maven &gt; Update project (forzando el check Update snapshots), es poco probable que eso lo solucione pero al menos te puede ayudar a encontrar el origen de la dependencia errónea.

Otro problema que te puede ocurrir cuando corras un launcher que te descargaste de nuestros ejemplos es que te aparezca un error de este tipo:

`Launch configuration references non-existing project celulares-ui-arena-scala`

En este caso el problema es que te descargaste el proyecto del SVN utilizando otro nombre que el que originalmente definimos. Entonces fijate cuál es el nombre del proyecto que está esperando y renombralo a ese, o bien entrá por el menú Run Configuration y apuntá el launcher al proyecto que vos definiste. Otra opción puede ser que no hayas ejecutado el comando mvn compile (Run As &gt; Maven build... Goal compile)

Otro problema es que al ejecutarlo te tire un error:

`java.util.NoSuchElementException: key not found: aop.opo.isolationLevel`
`   at scala.collection.MapLike$class.default(MapLike.scala:228)`
`   at scala.collection.AbstractMap.default(Map.scala:58)`
`   at scala.collection.mutable.HashMap.apply(HashMap.scala:64)`
`   at com.uqbar.apo.APOConfig$.getProperty(APOConfig.scala:27)`
`   at com.uqbar.apo.APOConfig.getProperty(APOConfig.scala)`
`   at org.uqbar.lacar.ui.impl.jface.bindings.DetailTransactionalObservableSet.`<init>` (DetailTransactionalObservableSet.java:17)`

(también podría ser otra key como apo.exclude.package).

Hay dos formas de resolverlo, mutuamente excluyentes:

#### Si usas Maven desde Eclipse

En ese caso deberías (dentro del Eclipse) dar un click derecho sobre el proyecto: Maven &gt; Update project chequeando la opción Update snapshots. Hacelo desde el plugin de Eclipse y no por la línea de comando.

#### Si usas la línea de comandos

-   Borrar todo el contenido de .m2/repository
-   Desde consola, ir al directorio del proyecto y usar el comando: mvn -U clean install eclipse:clean eclipse:eclipse
-   Importar el proyecto a eclipse
-   Click derecho sobre el proyecto &gt; build path &gt; configure build path, order and export y buscar arena-jface-3.3-SNAPSHOT.jar y hacer up hasta que quede debajo de las carpetas src
-   En el proyecto: Run As &gt; Java Application (en el ejemplo del conversor: ConversorWindow)

### Problemas específicos con Groovy

Si te aparece un error como el siguiente:

`groovy-all is loaded in version 2.0.6 and you are trying to load version 2.1.6`

Editá el pom.xml y cambiale la versión de groovy-all a 2.0.6 (o la versión en la que esté cargada)
