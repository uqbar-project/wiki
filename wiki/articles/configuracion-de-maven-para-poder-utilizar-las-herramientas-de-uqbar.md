---
layout: article
title: Configuracion de maven para poder utilizar las herramientas de uqbar
---

Para poder utilizar los artefactos Maven desarrollados por Uqbar debemos agregar los repositorios de donde bajar los artefactos de [Uqbar](http://www.uqbar-project.org) a la configuración de Maven. Esta configuración se debe indicar en el archivo _settings.xml_, que se ubica en:

- **Windows**: tu directorio de usuario + ".m2\settings.xml". Por ejemplo, si tu usuario es Juana, e instalaste Windows en el disco C:, debería estar en "C:\Users\Juana\.m2\settings.xml"
- **Linux**: tu directorio de usuario (~) + ".m2/settings.xml". Por ejemplo, si tu usuario es juana, debería estar en "~/.m2/settings.xml" o "/home/juana/.m2/settings.xml"

Dado que .m2 es un directorio oculto, debés activar la configuración para verlos. Te dejamos las instrucciones para [Windows](https://support.microsoft.com/en-us/help/4028316/windows-view-hidden-files-and-folders-in-windows-10) y [Linux](https://askubuntu.com/questions/470837/how-to-show-hidden-folders-in-ubuntu-14-04) 

El contenido de tu archivo _settings.xml_ debería quedar así:

```xml
 <settings xmlns="http://maven.apache.org/POM/4.0.0"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/settings-1.0.0.xsd">
   <profiles>
     <profile>
       <id>uqbar-wiki</id>
       <repositories>
         <repository>
           <id>uqbar-wiki.org-releases</id>
           <name>uqbar-wiki.org-releases</name>
           <url>http://maven.uqbar.org/releases</url>
         </repository>
         <repository>
           <snapshots/>
           <id>uqbar-wiki.org-snapshots</id>
           <name>uqbar-wiki.org-snapshots</name>
           <url>http://maven.uqbar.org/snapshots</url>
         </repository>
       </repositories>
     </profile>
   </profiles>
   <activeProfiles>
     <activeProfile>uqbar-wiki</activeProfile>
   </activeProfiles>
 </settings>
```
