---
layout: article
title: Introducción a Maven
---

Maven es una herramienta que ayuda a desarrollar un proyecto basado en el entorno de una JDK (Java, Xtend, Scala, Groovy, etc.)

## Funcionalidades

Maven cumple con las siguientes funciones principales que vamos a explicar en las siguientes secciones:

- Reificación de Proyecto / Artefacto en forma standard, declarativa y extensible
- Manejo de Dependencias
- Manejo del Ciclo de Vida del Artefacto, incluyendo releases
- Documentación y Comunicación

## Reificación de Proyecto/Artefacto

Java no trabaja la idea de proyecto, no lo representa como concepto. Entonces, cada uno de los IDEs pensados para Java agregan la idea de proyecto: en el caso de Eclipse tenemos

- el archivo `.classpath` que define los directorios donde compilar y las dependencias que necesita
- el archivo `.project` que contiene el nombre del proyecto, entre otras cosas

Si nosotros trabajamos con otro IDE (como IntelliJ IDEA o NetBeans) tenemos que adaptar estos archivos para generar el proyecto con sus dependencias adecuadamente. Maven permite trabajar entre IDEs con su propio modelo de proyecto, que se guarda en el archivo `pom.xml` (de **Project Object Model**)

## Identificación de un proyecto Maven

En el archivo pom se declaran, entre otras cosas, un identificador único de nuestro proyecto/artefacto, que resulta de la unión de tres identificadores:

- **groupId**: representa la organización autora/dueña del artefacto. Por ejemplo, los proyectos relacionados a las cátedras de Algoritmos 2 suelen usar el groupId `org.uqbar`.
- **artifactId**: este campo define el nombre por el que se conoce al proyecto en sí mismo. Ejemplo: commons-collections, eg-seguros-xtend, tp-futbol5-grupo01, etc.
- **versión**: es el último componente del nombre-rompecabezas, dado que groupId:artifactId denota un único proyecto pero no alcanza para definir en qué versión del proyecto nos estamos parando, se agrega un número de versión para completar la información que maven necesita para generar una identificación unívoca. Conviene seguir las reglas de [**versionado semántico**](https://semver.org/), para liberar versiones productivas. A veces se suele acompañar de un sufijo `RELEASE` (para versiones estables) o `SNAPSHOT` (para versiones intermedias que pueden estar sujetas a cambios)

A continuación un ejemplo básico.

```xml
<project xmlns="http://maven.apache.org/POM/4.0.0"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                          http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>org.uqbar</groupId>
    <artifactId>uqbar-commons</artifactId>
    <version>1.1.2</version>
</project>
```

> Cuando se publica un componente, se empaquetan todas las clases compiladas (`.class`) en un archivo comprimido que tiene la extensión `.jar` (de Java Archive). Opcionalmente podemos tener un archivo comprimido extra con los fuentes.

## Repositorios Maven

Cuando instalamos Maven, se crea un repositorio Maven local en una carpeta que por defecto suele ser `HOME/.m2`. Si queremos ubicar al componente cuyo identificador es `org.eclipse.xtend:org.eclipse.xtend.core:2.21.0.M1` podremos encontrarlo localmente en

```bash
%M2_HOME%/repository
   ├── org
   │   └── eclipse
   │         └── xtend
   │               └── org.eclipse.xtend.core
   │                      ├─ 2.19.0 (otra versión)
   │                      └─ 2.21.0.M1 --> dentro de esta carpeta estará el .jar y los fuentes
   └── settings.xml
```

- recordemos que el identificador de un componente se arma a partir del groupId + el artifactId + la versión
- se suele exportar como variable la carpeta `HOME/.m2` con el nombre `M2_HOME`
- dentro de HOME puede haber opcionalmente un archivo `settings.xml` que veremos más adelante
- en la subcarpeta `repository` están todos los componentes que descargamos en nuestro repositorio local

Vemos un video de ejemplo, en una máquina Linux. El comportamiento en una máquina Windows es exactamente igual, hay que explorar los directorios mostrando los que son ocultos, y navegar a partir de la carpeta de usuario\.m2:

![repo maven local](/img/wiki/maven_local_repo.gif)


## Relacionando proyectos maven desde el POM

Es posible referenciar a otros proyectos maven desde un POM, esto es muy útil para cuando necesitamos 

- declarar dependencias, 
- definir un proyecto padre, 
- agregar plugins

a la hora de compilar nuestro proyecto.

### Dependencias

Las dependencias se definen dentro de un tag `<dependencies>`:

```xml
<project ...>
	<...>
	<dependencies>
		<dependency>
			<groupId>org.junit.jupiter</groupId>
			<artifactId>junit-jupiter-params</artifactId>
			<version>5.5.2</version>
			<scope>test</scope>
		</dependency>
	</dependencies>
```

En este caso, estamos definiendo que nuestro proyecto tiene como pre-requisito el componente junit-jupiter-params asociado a org.junit.jupiter. Si trabajamos integrado a Eclipse:

- agregamos la dependencia en el pom, 
- actualizamos el proyecto (`Maven > Update project`)
- y eso dispara la descarga del componente al repositorio Maven local como vemos en el video

![descarga componente local](/img/wiki/downloading-maven-component.gif)

Al agregar una dependencia a un proyecto es posible especificar el `type` (por ej., `jar`), el `scope` (por ejemplo: test), y si es o no `optional`.

### Parent project

Otra variante es utilizar un proyecto padre mediante el tag `<parent>`, como ocurría en versiones anteriores de los ejemplos de Uqbar hasta 2019:

```xml
<parent>
    <groupId>org.uqbar-project</groupId>
    <artifactId>uqbar-xtend-parent</artifactId>
    <version>2.17.1</version>
</parent>
```

El parent project permite reutilizar definiciones comunes entre varios proyectos. En este caso particular, uqbar-xtend-parent (el nombre que le dimos a este artefacto) sirve para definir

- que utilizaremos la versión 2.17.0 de Xtend
- con una dependencia para correr tests unitarios
- compilando a JDK 1.8

### Plugins

Los plugins de Maven no solo permiten reutilizar lógica sino que además ejecutan acciones cuando son descargados. Así funciona el núcleo central de Maven: uno de los plugins más conocidos es `clean`, que elimina el directorio de destino (en el caso de Xtend, donde están los archivos Java y los .class que se generan a partir de los fuentes originales).

Distintos proyectos maven requieren/ofrecen distintos settings al ser referenciados como plugins. Veamos un ejemplo de la configuración del plugin de `xtend`:

```xml
		<plugin>
				<groupId>org.eclipse.xtend</groupId>
				<artifactId>xtend-maven-plugin</artifactId>
				<version>2.20.0</version>
				<executions>
					<execution>
						<goals>
							<goal>compile</goal>
							<goal>testCompile</goal>
						</goals>
						<configuration>
							<outputDirectory>${project.build.directory}/xtend-gen/main</outputDirectory>
							<testOutputDirectory>${project.build.directory}/xtend-gen/test</testOutputDirectory>
						</configuration>
					</execution>
				</executions>
			</plugin>
```

Aquí le estamos indicando a maven las características de la ejecución del plugin, es decir:

- Los goals (metas/capacidades del plugin). Este es un plugin de compilación por lo que indica los valores `compile` y `testCompile`.
- La configuración de las rutas del filesystem para los archivos java compilados.

Lo recomendable en cada caso es siempre revisar la documentación oficial del proyecto maven que queremos referenciar, para entender qué settings son requeridos o convenientes para nuestro proyecto.

## Dependencias transitivas

> Un detalle no menor de la resolución de dependencias de maven es que también funciona para las dependencias transitivas.

Por ejemplo:

- proyectoA --> proyectoB
- proyectoB --> proyectoC
- proyectoC --> proyectoD, proyectoE, proyectoF

Al resolver las dependencias, el proyectoA necesitará descargar los componentes B, C, D, E y F. Incluso podríamos requerir diferentes versiones de los mismos componentes.

Noten que un proyecto comercial "normal" o mediano, puede incluir decenas y hasta cientos de dependencias.

Eclipse permite integrar todas las definiciones de un pom junto con sus parents en la solapa `Effective POM`:

![effective POM](/img/wiki/effective-pom.gif)

## Repositorios remotos

TODO

### Definiendo repositorios remotos

TODO

## Ejecutando maven desde la consola

TODO

## Ciclos de build

TODO

## Resumen general de la arquitectura Maven

![maven architecture](/img/wiki/Maven2.png)

## Documentación oficial

Para más información recomendamos leer la documentación oficial del proyecto Maven:

- [POM reference](https://maven.apache.org/pom.html)
- [Introduction to the POM](https://maven.apache.org/guides/introduction/introduction-to-the-pom.html)

Y estos links:

- [Maven in 5 minutes](http://maven.apache.org/guides/getting-started/maven-in-five-minutes.html)
- [Simple Explanation of Maven - video de la Universidad de Cincinnati](https://www.youtube.com/watch?v=KNGQ9JBQWhQ)

## Links relacionados

- [Página principal de Algoritmos 2](algo2-temario.html)
