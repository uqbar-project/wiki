Hay varias cosas que es util configurar en Eclipse, independientemente del lenguaje que elijan.

Encoding
--------

Para no tener problemas con los tildes y demás caracteres especiales al bajarse los ejemplos conviene tener sincronizado el mismo encoding. Para eso, desde la barra de menú: Window &gt; Preferences, filtrar por "encoding" y cambiar todos a "UTF-8" o "ISO 10646/Unicode(UTF-8)". Por ejemplo: En General &gt; Workspace &gt; Text file encoding, seleccionar Other &gt; UTF-8. Aplicar cambios.

Spell
-----

Si van a programar en español, es recomendable desactivar el diccionario (viene por defecto en inglés). Para ello filtrar en el menú por la palabra "spell" y desactivar la corrección ortográfica (Spelling &gt; desactivar el check Enable spell checking). Aplicar cambios.

Otra opción es que se bajen un diccionario español de internet y lo configuren.

Warnings
--------

En varios lenguajes de la JVM, tendremos warning sobre serialización, una tecnología que no utilizaremos en la materia. Conviene desactivar el warning default de clases serializables que no definan un identificador de versión: Window &gt; Preferences, filtrar por "Serializable", solapa Java / Compiler / "Errors/Warnings", "Potential programming problems", y se setea el valor de "Serializable class without serialVersionUID" a Ignore. Aplicar cambios.

Indices de Maven
----------------

Si instalaron el plugin de Maven para Eclipse, o si ya viene instalado con el que hayan descargado, y no piensan utilizarlo, para ahorrar tiempos de carga es recomendable desactivar la inidización de artefactos. Esto es particularmente útil cuando tenemos conexión de red limitada o máquinas con pocos recursos.

Para eso, ir a Window -&gt; Preferences -&gt; Maven. Marcar Offline y desmarcar Download repository indexes

Filtros de paquetes
-------------------

Groovy, Java y Xtend comparten la forma de importar paquetes. En un JDK estándar hay muchos paquetes, y sólo usaremos unos pocos. Es recomendable indicarle a Eclipse que no nos sugiera paquetes que casi con seguridad no usaremos.

Para eso, en Java -&gt; Appearance -&gt; Type Filters, agregar las siguientes expresiones:

`   sun.*`
`   *.internal.*`
`   edu.emory.mathcs.backport.*`
`   java.awt.*`
`   java.swing.*`
`   org.omg.*`
