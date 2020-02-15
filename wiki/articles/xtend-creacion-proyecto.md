---
layout: article
title: Cómo generar un proyecto Xtend nuevo
categories: [xtend, nuevo proyecto, maven]
featured: true
---

Para realizar las prácticas, vas a crear un proyecto desde cero. Como ahora hay muchas más opciones, te dejamos una guía simple de cómo iniciarte.

# Crear proyecto Maven

Todos los ejemplos que vas a descargar de la materia, así como los proyectos en los que vas a trabajar, se basan en la tecnología Maven. Para crear un proyecto Maven, te dejamos esta animación:

![image](/img/languages/creatingNewProject.gif)

- Seleccionamos New > Project... Maven Project
- Chequeamos "Skip archetype selection" ya que no lo usaremos, y luego Next
- En la siguiente pantalla, escribimos cualquier cosa en "groupId", que es un valor que luego borraremos, pero que si lo dejamos vacío Eclipse nos mostrará un error. Y el nombre de nuestro proyecto o `artifactId`, éste sí es importante, seguimos la nomenclatura de paquetes de Java, para que no haya confusiones en los nombres: `ar.edu.unsam.prueba` identifica nuestro proyecto en todo el mundo
- cuando finalizamos, se genera un proyecto con un archivo pom.xml, que es fundamental para que Eclipse regenere el proyecto en otra máquina y descargue las dependencias

#  Configuración de un proyecto Maven

El acrónimo POM es por "Project Object Model". El archivo `pom.xml` es el core de la configuración de un proyecto maven. Es un solo archivo de configuración que contiene la mayoría de la información requerida para el build de un proyecto.

## Pom, pom, pom, pom...

Para comenzar a escribir la configuración del proyecto en un archivo `pom.xml` existen algunas opciones:

- Inicializar un proyecto maven desde 0 desde la terminal con el comando `mvn archetype:generate`. Al hacer esto maven nos pedirá los datos iniciales para inicializar el proyecto (artifactId, groupId, etc) y nos dejará un `pom.xml` creado y listo para usar/modificar.
- Crear un proyecto maven desde eclipse u algún otro IDE.
- Copiarse un `pom.xml` de un proyecto maven existente y adecuar los valores de los campos que identifican el proyecto.

Te dejamos entonces el modelo de proyecto Maven por defecto para la cursada 2020 de Algoritmos 2 (UNSAM): [pom.xml 2020 de ejemplo](pom-algo2-2020.xml). Luego tendrás que

- renombrar el archivo a `pom.xml`
- copiarlo dentro del directorio raíz de tu proyecto ya creado
- actualizar en base a los nombres de tu proyecto (el _artifactId_ y _groupId_)
- revisar las dependencias

Para profundizar un poco más sobre lo que contiene un archivo de Maven, te recomendamos que sigas leyendo este artículo.

## Yendo pom partes

### ID, please

Veamos lo que sería un `pom.xml` más minimalista posible:

```xml
    <project xmlns="http://maven.apache.org/POM/4.0.0"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                          http://maven.apache.org/xsd/maven-4.0.0.xsd">
      <modelVersion>4.0.0</modelVersion>

      <groupId>org.codehaus.mojo</groupId>
      <artifactId>my-project</artifactId>
      <version>1.0</version>
    </project>
```

Los dos primeros tags <project> y <modelVersion> es metadata que maven requiere para funcionar. Luego de eso, podemos encontrar la definición de los atributos `groupId:artifactId:version`, los cuales son requeridos para identificar el proyecto, actuando como una coordenada que define para Maven una versión específica de un proyecto.
  - groupId: es generalmente único dentro de la organización de un proyecto. Por ejemplo, los proyectos relacionados a las cátedras de Algoritmos 2 usan el groupId `org.uqbar-project`. Este valor puede usar (o no) la notación de puntos. En caso de utilizarla, esto conformará luego la ruta del repositorio donde se almacena el proyecto. En el caso del groupId `org.uqbar-project` este grupo 'vivirá' en el directorio $M2_REPO/org/uqbar-project.
  - artifactId: este campo define el nombre por el que se conoce al proyecto en sí mismo. Puede coincidir, aunque no es necesario, con el valor del groupId. Junto con el groupId este valor crea una clave compuesta que distingue al proyecto de cualquier otro proyecto en el universo de Maven. Junto con el groupId además, este valor define el path del proyecto en el repositorio.
  - version: es el último componente del nombre-rompecabezas, dado que groupId:artifactId denota un único proyecto pero no alcanza para definir en qué versión del proyecto nos estamos parando, se agrega un número de versión para completar la información que maven necesita para generar una identificación unívoca.

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

## Documentación oficial

Para más información recomendamos leer la documentación oficial del proyecto Maven:
- [POM reference](https://maven.apache.org/pom.html)
- [Introduction to the POM](https://maven.apache.org/guides/introduction/introduction-to-the-pom.html)

## Heredando atributos del POM de un parent project

Anteriormente la configuración de los proyectos Maven en la materia era gestionada a través de un `pom.xml` provisto por el equipo docente a través de un `parent project`.
Este recurso se ha deprecado, pero lo dejamos aquí para el archivo: [Actualizando el parent project](uqbar-parent-project-maven.html)

# Sincronización entre Eclipse y Maven

Cada vez que hagamos un cambio en el archivo `pom.xml`, nos aparecerá un mensaje de error en la solapa **Problems**, que se soluciona forzando la sincronización entre Eclipse y Maven (dado que cada uno maneja su propia estructura de proyectos Java). Como regla general, **siempre que necesitemos agregar alguna biblioteca, o dependencia, debemos hacerlo en el archivo pom y no desde las opciones que ofrece el Eclipse, porque nuestros compañeros o los docentes no tendrán esa biblioteca**. Para sincronizar Maven y Eclipse, nos paramos en el proyecto y con un botón derecho elegimos "Maven > Update Project".

# Primeros pasos

![image](/img/languages/firstClass.gif)

Ahora solo nos queda eliminar la línea groupId (con Ctrl + D), formatear el pom.xml (con Ctrl + Shift + F) y crear nuestra primera clase Perro. Es importante notar que tendremos dos carpetas donde ubicaremos los fuentes:

- `src/main/java`: donde irán las clases
- `src/test/java`: donde irán los tests

Por eso, nos ubicamos en `src/main/java` y con un botón derecho, New > Xtend Class (es importante que hayas configurado el Eclipse para que no esté escondida esta opción).

## Generación de archivos .java

En `src/main/generated-sources` vas a tener los archivos `.java` que se generan en base a los archivos de xtend. ¡No los toques! Porque cada cambio que hagas en tu clase xtend va a pisar los cambios de los archivos `.java`. En general no deberías mirar nunca el java que genera, porque además utiliza construcciones menos simples que si programaras directamente en java.

# Recomendaciones

A continuación te dejamos algunas recomendaciones para que tu estadía en Eclipse + Xtend sea más feliz:

- ¡Formatear el código! Nunca nos olvidemos de que nuestro código tiene que ser entendible para el resto de la humanidad. Además, el Eclipse lo hace solo (Ctrl + Shift + F).

![image](/img/languages/formattingCode.gif)

- Utilización de packages (paquetes). Es una buena práctica agrupar las clases afines en paquetes para organizar semánticamente el código. No hay una guía firme a seguir con respecto a cómo organizar nuestro código, ya que suele depender del contexto en el cual estamos trabajando, pero a medida que veas nuestros ejemplos y vayas haciendo las prácticas notarás que hay clases que se pueden agrupar en contextos similares. Te dejamos un ejemplo

```bash
proyecto
   ├── home
   ├── registration
   │   ├── Profile.xtend
   │   └── User.xtend
   └── settings
       ├── CustomPrivacy.xtend
       ├── DefaultPrivacy.xtend
       ├── Privacy.xtend
       └── Setting.xtend
```

De esta manera, logramos mayor granularidad en la organización de nuestras clases.
____

* [Siguiente paso: Sobre el control de versiones](xtend-amigandonos-git.html)
* [Volver al menú principal del entorno Xtend](xtend-principal.html)
