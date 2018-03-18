---
layout: article
title: Preparacion de un entorno de desarrollo xtend
categories: [xtend, entorno, instalacion]
featured: true
---

<img src="/img/wiki/Xtend-logo.png" height="30%" width="30%">

# Download e instalación base

## JDK

Descargá la última versión de la JDK 1.8 desde [esta dirección](http://www.oracle.com/technetwork/java/javase/downloads/index.html). 

> Asegurate de bajar una JDK que viene con el compilador. La versión debe ser Java 1.8 (todavía no tenemos un entorno estable para Java 1.9)

## Eclipse Oxygen

Ingresá a [esta dirección](https://www.eclipse.org/oxygen/) y descargá el Eclipse Oxygen for Java Developers (hay un botón Download).

## Maven

Seguí los pasos de instalación de [esta página](guia-de-instalacion-de-maven.html)

## Plugin Xtend 

Instalá el plugin de Xtend desde el Update Site, siguiendo estos pasos:

- En el menú de Eclipse, Help &gt; Install New Software ... botón Add
- En la ventana de diálogo Add Repository, en el nombre escribir algo como "Xtend Plugin" y en Location anotar http://download.eclipse.org/modeling/tmf/xtext/updates/composite/milestones/
- A partir del 2018 se estará usando la versión 2.13.0 que es la última, en caso de que vayan saliendo nuevas versiones, se puede elegir qué versión instalar destildando la opción "Show only the latest versions of available software" (más abajo está resaltado en la imagen)
- Seleccionar el check Xtend IDE, hacer click en Next y luego en Finish

<img src="/img/wiki/Xtend-installation.png" height="60%" width="60%">

- Reiniciar el Eclipse

<!-- -->

# Configuraciones default del eclipse

Antes que nada chequeá las [Configuraciones generales para cualquier Eclipse](configuraciones-generales-para-cualquier-eclipse.html)

# ¿Cómo empezar?

*  Crear un proyecto Maven (si no instalaste Maven hacelo como se sugiere [aquí](guia-de-instalacion-de-maven.html)
    * en la primera ventana, clickear en la opción "Create a simple project (Skip archetype selection)", luego Next...
    * definir un groupId, que puede ser el edu.*materia* . Ej: edu.dds, edu.algo2, etc.
    * definir un artifactId, que se asocia al nombre de tu proyecto

Para que compile el código xtend dentro de un proyecto hace falta tener una librería (en cada proyecto). La "famosa" *org.eclipse.xtext.xbase.lib*. En ese caso lo más fácil es que heredes de un pom de uqbar que ya hace el laburo por vos (ya declara las dependencias)

```xml
<parent>
     <groupId>org.uqbar-project</groupId>
     <artifactId>uqbar-xtend-parent</artifactId>
     <version>2.13.2</version>
</parent>
```

Esto lo podés hacer en la misma ventana del wizard que crea el proyecto Maven o bien editando el pom.xml de tu proyecto Maven recientemente creado. Luego boton derecho, "Maven" "Update Project..."

¿Dónde van las clases xtend?

* En src/main/java
* En src/main/generated-sources vas a tener los archivos .java que se generan en base a los archivos de xtend. ¡No los toques! Porque cada cambio que hagas en tu clase xtend va a pisar los cambios de los archivos .java. En general no deberías mirar nunca el java que genera, porque además utiliza construcciones menos simples que si programaras directamente en java.

<!-- -->

# Tips

* Para que cuando hagas New > File te aparezcan las clases y las interfaces Xtend, Window > Customize Perspective... > solapa Menu Visibility > expandís File > New > y seleccionás las de Xtend (Xtend class, inteface, annotation y enum).

<!-- -->

# Documentación

* [Documentación oficial](http://www.eclipse.org/xtend/documentation/)

<!-- -->

# Links útiles

* Si venís del mundo Java chequeá [este link](http://jnario.org/org/jnario/jnario/documentation/20FactsAboutXtendSpec.html)

