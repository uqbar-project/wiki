Algunas buenas prácticas para tener en cuenta:

Control de versiones
--------------------

-   No todos los archivos deben subirse al repo. Como regla general, no deberían subir archivos que se puedan generar a partir de otros, por ejemplo:
    -   los binarios que se generan a partir del código fuente de ustedes. Ocupan espacio en el repositorio y se corre el riesgo de estar laburando con versiones desactualizadas.
    -   archivos de configuración propios de cada uno (por ejemplo el .settings que genera el Eclipse). Si alguno tiene una configuración diferente (que es muy muy muy probable) la van a estar pisando a cada rato, e incluso es probable que les genere conflictos.

<!-- -->

-   Nunca deberían commitear sin explicar brevemente qué cambiaron. Si los mensajes son descriptivos (y “fix”, “asdsadsa” o “arreglo una cosita” definitivamente no lo son) rápidamente puedo detectar qué modificaron mis compañeros con sólo leer lo que escribieron en los commits. Una buena descripción me ayuda también a entender qué es lo que se modificó y por qué razón, especialmente útil a la hora de solucionar un conflicto o entender por qué se rompieron los tests.

<!-- -->

-   Establecer criterios de trabajo en grupo, algunos muy usados:
    -   los tests tienen que estar en verde
    -   los tests son de todos y todos somos responsables por mantenerlos
    -   si encontramos un bug y no había un test que lo probaba agregamos uno
    -   los tests son rápidos de correr

<!-- -->

-   Establecer formas de trabajo y organizar el trabajo nuestro con el de los demas:
    -   Cuando empiezo el día primero me sincronizo con el repositorio para ver los cambios que no tengo en el código
    -   Acepto los cambios entrantes y en caso de ser necesario resuelvo conflictos
    -   Corro los tests y veo que todo anda sobre ruedas
    -   Me vuelvo a sincronizar y veo que ya no quedan ni conflictos ni cambios sin aceptar
    -   Subo mis cambios al repositorio remoto para que mis compañeros lo vean

Esto mismo lo hacemos varias veces al día y antes de subir algo nuevo al repositorio. Siempre corro los tests y si alguno da error, bueno, alguien subió algo indebido. Los Tests y el repositorio nos ayudan a entender cuándo se rompió y por qué.

### SVN

-   Toda la estructura de carpetas de su proyecto debe estar contenida dentro del directorio *trunk*. Si bien SVN nos permite trabajar fuera de él, todas las funcionalidades que nos brinda (taggear, branchear, etc) suponen que nosotros respetamos esta convención. Si tenemos el código esparcido por todas partes lo vamos a tener que hacer a mano... y entonces la herramienta deja de tener sentido. Otra contra: al introducir nuestro propio estándar nos abrimos de la comunidad, eso nos obliga a tener que explicarle al resto qué convenciones seguimos (si es que seguimos alguna) y reduce la felicidad de aquellos que empiecen a colaborar en nuestro proyecto.

<!-- -->

-   Si están utilizando Tortoise o el plugin de subversion para Eclipse, ambos van a tener la opción “add to <svn:ignore>”. En el caso de hacerlo por consola pueden correr el comando “svn propset <svn:ignore> <nombre_de_Archivo>” o “svn propset <svn:ignore> .” si lo que quieren es ignorar todo un directorio.

Tener en cuenta que los archivos o directorios que ya fueron subidos por alguien no se pueden ignorar. En este caso, si alguien los subió por error, lo que se debe hacer es: borrar la carpeta “en el server” luego hacer update en mi máquina (lo cual me va a borrar también esa carpeta) finalmente, si la carpeta vuelve a aparecer (por ejemplo, la carpeta “target” que se genera automáticamente o “classes”, etc), puedo ahora ignorarla como se contó antes Para el paso 1, es decir borrar la carpeta del server, desde eclipse se puede hacer cambiando la perspectiva a “SVN Repository Exploring”, ahí lo que verán en la vista de la izquierda son los repositorios. Se pueden navegar y botón-derecho-&gt;Delete, borra del servidor (de nuevo acordarse de escribir un comentario digno que explique lo que estamos haciendo :P)

### Git

-   A la hora de ignorar archivos, git nos provee una forma muy sencilla y a la vez poderosa de hacerlo: los .gitignore. Estos son archivos que podemos crear en cualquier carpeta de nuestra estructura (usualmente en el directorio raíz) y especificar en ellos qué archivos o patrones deberían quedar fuera del versionado. Cada línea del .gitignore representa algo que queremos ignorar, por ejemplo "enunciado.pdf" nos ignoraría ese archivo, mientras que "\*.class" va a ignorar todos los .class que tengamos en el directorio actual y en todos sus subdirectorios. Pueden encontrar versiones de .gitignore para la mayoría de los lenguajes en <https://github.com/github/gitignore>

Eclipse
-------

-   Cómo organizar los archivos: Convention Over Configuration <http://en.wikipedia.org/wiki/Convention_over_configuration>, el código productivo debería estar en src/main y el código de test en src/test. Qué gano usando estas convenciones? Me corren los tests, me integro con el mundo y eventualmente puedo usar herramientas externas sin tener que configurar nada, ya que respetan estas convenciones (como Maven).

<!-- -->

-   Formatear el código! Nunca nos olvidemos de que nuestro código tiene que ser entendible para el resto de la humanidad. Además, el Eclipse lo hace solo (Ctrl + Shift + F).

<!-- -->

-   Avisarle al Eclipse que mis carpetas son "source folders". Esto le da una semántica a mis archivos y el tipo se aviva de que en verdad tengo paquetes y clases, en vez de archivos y carpetas (de hecho él se encarga de generarme esas carpetas).

