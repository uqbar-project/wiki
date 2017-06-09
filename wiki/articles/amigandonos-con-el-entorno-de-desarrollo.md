---
layout: article
title: Amigandonos con el entorno de desarrollo
---

Algunas buenas prácticas para tener en cuenta:

Control de versiones
--------------------

-   No todos los archivos deben subirse al repo. Como regla general no deberían subir archivos que se puedan generar a partir de otros, por ejemplo:
    -   los binarios que se generan a partir del código fuente de ustedes. Ocupan espacio en el repositorio y se corre el riesgo de estar trabajando con versiones desactualizadas.
    -   archivos de configuración propios de cada uno (por ejemplo el .settings que genera el Eclipse). Si alguno tiene una configuración diferente (que es muy muy muy probable) la van a estar pisando a cada rato, e incluso es probable que les genere conflictos.


<!-- -->

-   Nunca deberían subir nuevos fuentes al repositorio sin explicar brevemente qué cambiaron. Si los mensajes son descriptivos (y “fix”, “asdsadsa” o “arreglo una cosita” definitivamente no lo son) rápidamente puedo detectar qué modificaron mis compañeros con sólo leer lo que escribieron en los commits. Una buena descripción me ayuda también a entender qué es lo que se modificó y por qué razón, especialmente útil a la hora de solucionar un conflicto o entender por qué se rompieron los tests.

<!-- -->

-   Establecer criterios de trabajo en grupo, algunos muy usados:
    -   los tests tienen que estar en verde
    -   los tests son de todos y todos somos responsables por mantenerlos
    -   si encontramos un bug y no había un test que lo probaba agregamos uno
    -   los tests son rápidos de correr

<!-- -->

-   Establecer formas de trabajo y organizar el trabajo nuestro con el de los demás:
    -   Cuando empiezo el día primero sincronizo el repositorio para ver los cambios que no tengo en el código
    -   Acepto los cambios entrantes y en caso de ser necesario resuelvo conflictos
    -   Corro los tests y veo que todo anda sobre ruedas
    -   Vuelvo a sincronizar y veo que ya no quedan ni conflictos ni cambios sin aceptar
    -   Subo mis cambios al repositorio remoto para que mis compañeros lo vean

Esto mismo lo hacemos varias veces al día y antes de subir algo nuevo al repositorio. Siempre corro los tests y si alguno da error, bueno, alguien subió algo indebido. Los tests y el repositorio nos ayudan a entender cuándo se rompió y por qué.

<!-- -->

### Git

-   A la hora de ignorar archivos, git nos provee una forma muy sencilla y a la vez poderosa de hacerlo: los .gitignore. Estos son archivos que podemos crear en cualquier carpeta de nuestra estructura (usualmente en el directorio raíz) y especificar en ellos qué archivos o patrones deberían quedar fuera del versionado. Cada línea del .gitignore representa algo que queremos ignorar, por ejemplo "enunciado.pdf" nos ignoraría ese archivo, mientras que "\*.class" va a ignorar todos los .class que tengamos en el directorio actual y en todos sus subdirectorios. Pueden encontrar versiones de .gitignore para la mayoría de los lenguajes en <https://github.com/github/gitignore>

A continuación te damos una lista de posibles recursos que deberían estar en el archivo .gitignore:

-   /target/
-   .classpath
-   .project
-   bin
-   generated*
-  .settings


Eclipse
-------

-   Cómo organizar los archivos: [Convention Over Configuration](http://en.wikipedia.org/wiki/Convention_over_configuration), el código productivo debería estar en src/main y el código de test en src/test. ¿Qué gano usando estas convenciones? Me corren los tests, me integro con el mundo y eventualmente puedo usar herramientas externas sin tener que configurar nada, ya que respetan estas convenciones (como Maven).

<!-- -->

-   ¡Formatear el código! Nunca nos olvidemos de que nuestro código tiene que ser entendible para el resto de la humanidad. Además, el Eclipse lo hace solo (Ctrl + Shift + F).

<!-- -->

-   Utilización de packages (*paquetes*). Es una buena práctica agrupar las clases afines en paquetes para organizar semánticamente el código. No hay una guía firme a seguir con respecto a cómo organizar nuestro código, ya que suele depender del contexto en el cuál estamos trabajando, pero es muy común respetar convenciones para mantener la simplicidad y flexibilidad ahorrando al desarrollador de tomar estas decisiones (“*Convention over Configuration*”).

Por ejemplo, en un proyecto completo tener paquetes para el “*dominio*”, “*controllers*” y “*vista*” (si correspondiese) es una convención común. Cada uno de éstos agrupa las clases que tienen un concepto afín. Se podría seguir ahondando en la definición de subpaquetes agrupando, por ejemplo, por componente:

```bash
domain/
   ├── home
   ├── registration
   │   ├── Profile.java
   │   └── User.java
   └── settings
       ├── CustomPrivacy.java
       ├── DefaultPrivacy.java
       ├── Privacy.java
       └── Setting.java

```

De esta manera, logramos mayor granularidad en la organización de nuestras clases.

Otro uso de los paquetes, también relacionado con el concepto anteriormente mencionado Convention over Configuration, es el de identificar unívocamente a una aplicación. ¿Qué significa ésto? Que las clases que yo defina formen parte de un meta grupo que los identifique, y así evitar colisiones en los nombres que yo les ponga. Por ejemplo: yo puedo definir la clase Color, pero la api de Java AWT ya define una clase Color.

Es por este motivo que se utiliza el dominio de internet de la organización, pero “dado vuelta”. Por ejemplo si trabajamos para Google sería común encontrar paquetes del estilo **com.google.blah**. Para el ejemplo de Color, la api de Java la define como java.awt.Color. En nuestro caso podríamos usar, por ejemplo: **ar.edu.materiaQueCursan.paquete**.
