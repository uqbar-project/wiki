---
layout: article
title: Configuraciones generales para cualquier eclipse
---

Hay varias cosas que es útil configurar en Eclipse, independientemente del lenguaje que elijan.

# Configuraciones adicionales de Eclipse

## Installed JRE

![image](/img/languages/installedJREConfigEclipse.png)

Una de las configuraciones más importantes, es el compilador que estará usando Eclipse para nuestro código Xtend. En el ejemplo de arriba, hemos configurado un compilador de Java 1.11 (aunque nosotros recomendamos que utilicen el 1.8). Es preciso recalcar que **deben apuntar a una JDK (con el compilador) y no a una JRE o no podrán ejecutar ningún programa Xtend, ni Java**.

A continuación te dejamos una animación que muestra cómo configurar otra JDK, para lo cual hay que pasar el directorio raíz (no el `bin` donde residen el compilador y los demás programas de Java).

![image](/img/languages/jdkConfigEclipse.gif)

## New > Artefactos de Xtend

Para que cuando hagas New > File te aparezcan las clases y las interfaces Xtend, Window > Customize Perspective... > solapa Menu Visibility > expandís File > New > y seleccionás las de Xtend (Xtend class, inteface, annotation y enum).

![image](/img/languages/customizeXtendPerspective.gif)

## Compiler

Algunas versiones de Eclipse utilizan por defecto compatibilidad con el compilador Java 1.4, algo que no es conveniente si vamos a trabajar con herramientas como Generics o Annotations que vienen a partir del JDK 1.5.

Para esto deben ir a Window Preferences &gt; Java &gt; Compiler &gt; y donde dice JDK Compliance subir la propiedad "Compiler compliance level" de 1.4 a una superior (1.8 ó superior)

En caso contrario al bajar proyectos compilados en JDKs superiores aparecerán mensajes de error como estos:

```bash
Syntax error, annotations are only available if source level is 1.5 or greater
Syntax error, parameterized types are only available if source level is 1.5 or greater
```

## Encoding (sólo Windows)

![image](/img/languages/encodingConfigEclipse.png)

Para no tener problemas con los tildes y demás caracteres especiales al bajarse los ejemplos conviene tener sincronizado el mismo encoding. Para eso, desde la barra de menú: Window &gt; Preferences, filtrar por "encoding" y cambiar todos a "UTF-8" o "ISO 10646/Unicode(UTF-8)". Por ejemplo: En General &gt; Workspace &gt; Text file encoding, seleccionar Other &gt; UTF-8. Aplicar cambios.

## Spell

![image](/img/languages/spellConfigurationEclipse.png)

Si van a programar en español, es recomendable desactivar el diccionario (viene por defecto en inglés). Para ello filtrar en el menú por la palabra "spell" y desactivar la corrección ortográfica (Spelling &gt; desactivar el check Enable spell checking). Aplicar cambios.

Otra opción es que se bajen un diccionario español de internet y lo configuren.

## Warnings

![image](/img/languages/potentialProgrammingProblemsConfigEclipse.png)

En varios lenguajes de la JVM nos aparecerá una molesta advertencia sobre la serialización de clases, algo que por el momento no necesitamos. Conviene desactivar el warning default de clases serializables que no definan un identificador de versión: Window &gt; Preferences, filtrar por "Serializable", solapa Java / Compiler / "Errors/Warnings", "Potential programming problems", y se setea el valor de "Serializable class without serialVersionUID" a Ignore. Aplicar cambios.

Opcionalmente, nosotros recomendamos subir a "Warning" estas dos configuraciones

- "Potential null pointer access"
- "Redundant null check"

## Shortcuts

En algunas distribuciones de Linux existe un shortcut por defecto que es Ctrl + Space, que colisiona con el shortcut del content assist de Eclipse . Para solucionar el problema, hay que deshabilitar el binding: en Ubuntu: System Settings -&gt; Keyboard-&gt; Shortcuts en Lubuntu: click en el logo que esta abajo a la izquierda -&gt; Preferencias -&gt; Metodos de entrada por Teclado y se cambia a "Disabled"

## Configuración de Maven

Window &gt; Preferences &gt; Maven, debe tener esta configuración:

![image](/img/languages/mavenConfigurationEclipse.png)

- `Download repository index on startup`, al igual que `Update Maven projects on startup`  deben estar destildados para evitar demoras al iniciar tu Eclipse
- Por el contrario, `Download Artifact Sources` y `Download Artifact JavaDoc` deben estar tildados, porque eso descargará documentación y fuentes de los componentes que uses, algo bastante útil cuando necesitamos solucionar un error o entender de qué manera comunicarnos con él.
- la marca `Offline` debe estar destildada, o no intentará conectarse a Internet para bajar componentes.

## Filtros de paquetes

En un JDK estándar hay muchos paquetes, y sólo usaremos unos pocos. Es recomendable indicarle a Eclipse que no nos sugiera paquetes que casi con seguridad no usaremos.

Para eso, en Java &gt; Appearance &gt; Type Filters, agregar las siguientes expresiones:

```bash
bash
sun.*
*.internal.*
edu.emory.mathcs.backport.*
java.awt.*
java.swing.*
org.omg.*
```

___
