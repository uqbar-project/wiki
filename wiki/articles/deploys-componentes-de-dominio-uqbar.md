---
layout: article
title: Deploy componentes Uqbar
---


# Componentes de dominio Uqbar

## Motivación

Los componentes de dominio uqbar están pensados para 

- trabajar exclusivamente el modelo de negocio de los ejemplos...
- ... y poder reutilizarlos en diferentes tecnologías, ya sea de UI (Arena, Wicket, incluso Android) o de lenguajes base (podríamos definir el conversor en Java y generar la UI en Arena programándola en Scala, Xtend o Groovy)

# Configuraciones de Maven necesarias

## Parent-project

El proyecto padre debe ser

- uqbar-examples-parent/ para proyectos Java
- uqbar-examples-xtend-parent/ para proyectos Xtend

Cualquiera de estos proyectos padre están deployados en el repositorio Maven de Uqbar, que está situado en

http://uqbar-wiki.org/mvn/releases


## Dependencias

En el pom debemos tener dependencias hacia

- uqbar-domain (la versión correspondiente)

y no es necesario nada más.

Los objetos de dominio deben anotarse como Observable para que funcionen en Arena, y respetar la convención Java Bean de tener un constructor vacío y getters/setters.

## Repositorios

Dado que el proyecto padre no está en Maven Central, debemos agregar los repositorios maven de Uqbar en forma manual:

```xml
	<repositories>
		<repository>
			<id>uqbar-wiki.org-releases</id>
			<name>uqbar-wiki.org-releases</name>
			<url>http://uqbar-wiki.org/mvn/releases</url>
		</repository>
		<repository>
			<snapshots />
			<id>uqbar-wiki.org-snapshots</id>
			<name>uqbar-wiki.org-snapshots</name>
			<url>http://uqbar-wiki.org/mvn/snapshots</url>
		</repository>
	</repositories>
```

Esto es para que el build de travis no falle.

# Deploy de una nueva versión

Dado que no tenemos CI en este tipo de ejemplos, debemos manualmente

- Definir la versión del ejemplo, por caso el conversor podría pasar de 1.0.5-SNAPSHOT a 1.0.5
- Hacer un commit + push al repositorio git
- Una vez que esté el ok de Travis...
- ...en la línea de comando ejecutar

```bash
$ mvn clean deploy
```

- Esto agregará el componente en http://uqbar-wiki.org/mvn/releases.
- Por último, conviene subir la versión del ejemplo a 1.0.6-SNAPSHOT, para estar seguros de no subir un release.

A futuro esperamos contar con un esquema de CI que facilite las cosas.
