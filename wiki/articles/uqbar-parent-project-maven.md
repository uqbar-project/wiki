---
layout: article
title: Actualizando el parent project
categories: [xtend, nuevo proyecto, maven]
featured: true
---

# Actualizando el parent project

- si descargamos previamente algún otro proyecto, podemos copiar el **parent project**, o bien podés copiarlo aquí

```xml
<parent>
    <groupId>org.uqbar-project</groupId>
    <artifactId>uqbar-xtend-parent</artifactId>
    <version>2.17.1</version>
</parent>
```

El parent project permite reutilizar definiciones comunes entre varios proyectos. En nuestro caso, uqbar-xtend-parent (el nombre que le dimos a este artefacto) sirve para definir

- que utilizaremos la versión 2.17.0 de Xtend
- con una dependencia para correr tests unitarios
- compilando a JDK 1.8
- y muchas otras cosas más que iremos viendo poco a poco

Lo podés encontrar buscando en esta URL: https://search.maven.org/, más concretamente https://search.maven.org/search?q=uqbar-xtend. Es un componente que hemos puesto en la nube de los repositorios de Maven para simplificarle un poco la tarea inicial a ustedes.
