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

Consideraciones para un proyecto Grails + Xtend
-----------------------------------------------

-   En cada proyecto, recomendamos crear un Source Folder específico para las clases Xtend (puede ser src/xtend).
-   Obviamente, evitar que haya dos clases (una en Java/Groovy y otra en Xtend) con el mismo nombre (esto es, dentro de un mismo package, por más que estén en distintos source folders esto va a traer conflictos)
-   Para compilar el proyecto en Eclipse, hay que hacer Project &gt; Properties, en Build Path &gt; solapa Libraries &gt; Add Library &gt; Xtend Library
-   En el archivo BuildConfig.groovy de cada proyecto, hay que incorporar la dependencia con las librerías propias de Xtend para que en la aplicación se incluya la librería xtend:

`repositories {`
`    ...`
`    mavenLocal()`
`    mavenCentral()`
`    // uncomment these (or add new ones) to enable remote dependency resolution from public Maven repositories`
`    mavenRepo "`[`http://repo.maven.apache.org/maven2/`](http://repo.maven.apache.org/maven2/)`"`
`    ...`
`}`
`dependencies {`
`    // specify dependencies here under either 'build', 'compile', 'runtime', 'test' or 'provided' scopes e.g.`
`    build 'org.eclipse.xtext:org.eclipse.xtext.xbase.lib:2.4.2'`
`}`

Links
-----

-   Volver a [\[\[Instalación\_de\_Entorno\_Web\_Grails](--instalacion-de-entorno-web-grails.html)

