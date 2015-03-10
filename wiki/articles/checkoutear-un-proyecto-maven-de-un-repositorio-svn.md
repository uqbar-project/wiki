Este artículo asume la presencia de un entorno de trabajo con una JDK. En caso de no contar con un repositorio de esas características conviene leer las instrucciones para la [preparación de un entorno de trabajo Java](preparacion-de-un-entorno-de-trabajo-java.html).

También se asume la preexistencia de un proyecto mavenizado y publicado en un repositorio SVN, si lo que se desea es crear el proyecto en lugar de checkoutearlo, aquí están las instrucciones para la [creación de un proyecto maven básico](creacion-de-un-proyecto-maven-basico.html) y su posterior publicación en el repositorio.

El proceso tiene los siguientes pasos, que se detallan a continuación:

-   Checkout, es decir, obtener el código del proyecto desde el repositorio y alojarlo en nuestro espacio de trabajo local.
-   Adaptación del proyecto maven para ser utilizado dentro del entorno Eclipse.

Checkout
--------

El checkout se puede hacer tanto desde el eclipse como desde un cliente svn independiente. En ambos casos se debe contar con la URL del repositorio en el que se publicó el proyecto, por ejemplo:

[`http://svn2.xp-dev.com/svn/mi_repositorio/mi_primer_proyecto/trunk`](http://svn2.xp-dev.com/svn/mi_repositorio/mi_primer_proyecto/trunk)`.`

Checkout desde el Eclipse
-------------------------

En caso de hacerlo desde el eclipse, la forma de hacerlo es:

-   Ir a la perspectiva "SVN Repository Exploring"
-   En la vista "SVN Repositories" hacer click derecho sobre algún espacio en blanco y ahí elegir la opción "New" &gt; "Repository Location"
-   Ingresar la URL del repositorio svn y aceptar

Si varios proyectos comparten el mismo repositorio, una opción válida es definir el Repositorio de SVN desde una carpeta padre. *Ejemplo:* <http://svn2.xp-dev.com/svn/mi_repositorio/>

Eso permitirá navegar en el árbol de proyectos del repositorio padre en lugar de definir un repositorio cada vez que nos interese bajarnos un proyecto particular.

Una vez hecho todo esto se debe elegir Checkout &gt; "Check out as a project in the workspace... Project name:" y elegir el nombre del proyecto para ubicarlo en el workspace del eclipse. Dejar por defecto todas las otras opciones como están.

### Adaptar un proyecto maven para ser usado desde el Eclipse

Una vez realizado el checkout, desde el mismo entorno botón derecho sobre el proyecto &gt; Configure &gt; Convert to Maven project. O si preferís desde la consola podés hacer

`mvn eclipse:eclipse`

Checkout desde la consola en Linux
----------------------------------

Parados sobre el directorio del workspace de eclipse se ejecuta:

`svn co `<url-del-repo-svn>` `<nombre-del-proyecto>

Por ejemplo:

`svn co `[`http://svn2.xp-dev.com/svn/mi_repositorio/mi_primer_proyecto/trunk`](http://svn2.xp-dev.com/svn/mi_repositorio/mi_primer_proyecto/trunk)` mi_primer_proyecto`

El último parámetro indica el nombre que tendrá el proyecto en el directorio local, lo normal sería utilizar el mismo nombre del proyecto. Es importante indicarlo porque en caso contrario el default va a ser `trunk` y no es útil.

### Adaptar un proyecto maven para ser usado desde el Eclipse

Una vez hecho todo esto se debe importar el proyecto en el eclipse (import &gt; existing maven project y buscar el archivo pom.xml dentro del proyecto).
