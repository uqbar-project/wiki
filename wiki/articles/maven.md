---
layout: article
title: Introducción a Maven
---

Maven es una herramienta que ayuda a desarrollar un proyecto basado en el entorno de una JDK (Java, Xtend, Scala, Groovy, etc.)

### Funcionalidades

Maven cumple con las siguientes funciones principales que vamos a explicar en las siguientes secciones:

- Reificación de Proyecto / Artefacto en forma standard, declarativa y extensible
- Manejo de Dependencias
- Manejo del Ciclo de Vida del Artefacto, incluyendo releases
- Documentación y Comunicación

### Reificación de Proyecto/Artefacto

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

## Repositorios Maven

Para entender dónde se publican esos componentes presentaremos el concepto de repositorio Maven.






 En el caso del groupId `org.uqbar-project` este grupo 'vivirá' en el directorio $M2_REPO/org/uqbar-project.




### Relacionando proyectos maven desde el POM

Es posible referenciar a otros proyectos maven desde un POM, esto es muy útil para cuando necesitamos declarar dependencias, definir un proyecto padre, agregar plugins.

Al usar estos tags (`<dependencies>`, `<parent>`, `<plugins>`) debemos tener presentes algunas cosas:
- Para referenciar un proyecto maven siempre tenemos que indicar las coordenadas (los atributos `groupId:artifactId:version`).
- Al agregar una dependencia a un proyecto es posible especificar el `type` (por ej., `jar`), el `scope` (por ejemplo: test), y si es o no `optional`,
- Distintos proyectos maven requieren/ofrecen distintos settings al ser referenciados como plugins. Veamos un ejemplo de la configuración del plugin de `xtend`:

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


## Resumen general de la arquitectura Maven

```
%M2_HOME%
   ├── org
   │   ├── uqbar
   │   └── apache
   │         └── commons
   │               └── commons-lang3
   │                      ├─ 3.3.2
   │                      └─ 3.8.1
   └── settings.xml
```

![maven architecture](/img/wiki/Maven2.png)

## Documentación oficial

Para más información recomendamos leer la documentación oficial del proyecto Maven:
- [POM reference](https://maven.apache.org/pom.html)
- [Introduction to the POM](https://maven.apache.org/guides/introduction/introduction-to-the-pom.html)


## Links relacionados

- [Página principal de Algoritmos 2](algo2-temario.html)




