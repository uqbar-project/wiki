Pre-requisitos
--------------

-   Tenés que haber instalado el framework Grails en tu máquina, si no lo hiciste revisá [este link](instalacion-de-entorno-web-grails.html)

Plugin de Xtend
---------------

Lleva dos pasos

-   Entrando a la dirección <http://www.eclipse.org/xtend/download.html>, se copia la URL del Update Site &gt; Latest Release (o cualquier otro release que quieran ustedes, nuestra recomendación es instalar el último release estable)
-   Dentro del STS, ir a Help &gt; Install New Sofware..., en el texto Work with pegar la URL del paso anterior y descargar solamente el último plugin de Xtend (salvo que se quiera trabajar con Xtext)

Una vez avanzado sobre el asistente, se reinicia el entorno.

Configuraciones adicionales
---------------------------

Además de las configuraciones [sugeridas para Grails](instalacion-de-entorno-web-grails-configuraci-c3-b3n-del-entorno-grails.html), necesitás:

-   especificar por default el directorio donde el compilador Xtend deja los .java generados, para que el framework Grails los pueda ver. Entonces en Window &gt; Preferences &gt; Xtend &gt; Compiler cambiamos el texto *directory* a "java" en lugar de "xtend-gen".

Consideraciones para un proyecto Grails + Xtend en el mismo proyecto
--------------------------------------------------------------------

-   En cada proyecto, recomendamos crear un Source Folder específico para las clases Xtend (puede ser src/xtend).
-   Obviamente, evitar que haya dos clases (una en Java/Groovy y otra en Xtend) con el mismo nombre (esto es, dentro de un mismo package, por más que estén en distintos source folders esto va a traer conflictos)
-   Para compilar el proyecto en Eclipse, hay que hacer Project &gt; Properties, en Build Path &gt; solapa Libraries &gt; Add Library &gt; Xtend Library
-   En el archivo BuildConfig.groovy de cada proyecto, hay que incorporar la dependencia con las librerías propias de Xtend para que se incluya en la aplicación cuando se haga el deploy en el web server. Para esto hay que hacer dos cosas: 1) definir como repositorio maven "<http://repo.maven.apache.org/maven2>", y 2) agregar una dependencia a la versión de xtend que queramos.

`repositories {`
`    ...`
`    mavenLocal()`
`    mavenCentral()`
`    // uncomment these (or add new ones) to enable remote dependency resolution from public Maven repositories`
`    mavenRepo "`[`http://repo.maven.apache.org/maven2/`](http://repo.maven.apache.org/maven2/)`"    // punto 1`
`    ...`
`}`
`dependencies {`
`    // specify dependencies here under either 'build', 'compile', 'runtime', 'test' or 'provided' scopes e.g.`
`    build 'org.eclipse.xtend:org.eclipse.xtend.lib:2.6.1'  // punto 2`
`}`

Otra opción puede ser definir esta dependencia:

`dependencies {`
`    // specify dependencies here under either 'build', 'compile', 'runtime', 'test' or 'provided' scopes e.g.`
`    build 'org.eclipse.xtend:org.eclipse.xtend.standalone:2.6.1'`
`}`

**Nota:** 2.6.1 es en realidad la versión de xtend que se quiera utilizar. Se puede buscar en el repositorio Maven las versiones disponibles desde esta URL: <http://search.maven.org/#browse>, y ahí buscan org.eclipse.xtend.lib. En los resultados les figurará la última versión o bien versiones anteriores a las que pueden apuntar, a partir del link all(xxx).

Consideraciones para proyectos Grails + Xtend por separado
----------------------------------------------------------

En el proyecto Xtend -de dominio, asumimos- tenés que definir un source folder "src/main/xtend". Por las window preferences el compilador Xtend utilizará "java" como directorio donde dejar los fuentes. Esto significa que si escriben sus fuentes en "src/main/xtend" en el directorio "src/main/java" estarán los equivalentes java para que luego sean empaquetados en un jar mediante un mvn install.

Luego en el proyecto de ui Grails hay que

-   correr un mvn install (Run &gt; Maven install) para que lo incorpore al repositorio maven local
-   referenciar en compilación al proyecto de dominio en Xtend, esto se hace descargando el proyecto dentro del STS, o bien agregando el .jar como librería dentro del proyecto: copian el jar en un directorio \\lib del proyecto ui y luego hacen Build path &gt; Add jar, apuntando a la dirección relativa del .jar
-   para correr la aplicación en el servidor, referenciar en el Build Config la dependencia al .jar previamente instalado en el repositorio local, respetando group id + artifact id + version

Como migrar un proyecto existente de Arena a Grails
---------------------------------------------------

Si ya desarrollaste un proyecto en Arena y querés aprovechar el modelo de la vista (objetos de dominio, repos/homes, application models, etc.) para trabajarlo en Grails, te dejamos dos recomendaciones

-   No trates de checkoutear el proyecto desde el GGTS y utilizar la herramienta "Convert to Grails project".
-   Es probable que tengas que hacer algunas modificaciones mínimas a esos objetos, entonces lo más conveniente para no tener que trabajar en dos entornos es integrar en el mismo proyecto lo que ya hiciste para Arena + la vista de Grails.

Te dejamos un instructivo donde los pasos son:

1.  New &gt; Grails project &gt; darle un nombre al proyecto
2.  Crear un source folder src/xtend
3.  Copiar los packages de xtend del proyecto original a la carpeta src/xtend (se puede hacer copiando los archivos del filesystem)
4.  Eliminar los packages que sean de interfaz de usuario (ui, runnable, application, etc.)
5.  Modificar el BuildConfig para incorporar la librería xtend y el paquete uqbar-domain

`dependencies {`
`    build 'org.eclipse.xtend:org.eclipse.xtend.lib:2.6.1'`
`    build "org.uqbar-project:uqbar-domain:3.4-SNAPSHOT"`
`}`

1.  Posicionarse en el proyecto, botón derecho &gt; Grails Tools &gt; Refresh Dependencies
2.  De no encontrar los jars de uqbar-domain, se puede agregar estas líneas en el BuildConfig.groovy:

`repositories {`
`    mavenRepo "`[`http://uqbar-wiki.org/mvn/snapshots`](http://uqbar-wiki.org/mvn/snapshots)`"`
`    mavenRepo "`[`http://uqbar-wiki.org/mvn/releases`](http://uqbar-wiki.org/mvn/releases)`"`

1.  Ya se puede correr la aplicación con run-app

Links
-----

-   Volver a [Instalación\_de\_Entorno\_Web\_Grails](instalacion-de-entorno-web-grails.html)

