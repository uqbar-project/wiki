---
layout: article
title: Configuracion de maven para poder utilizar las herramientas de uqbar
---

Para poder utilizar los artefactos Maven desarrollados por Uqbar debemos agregar los repositorios de donde bajar los artefactos de [Uqbar](http://www.uqbar-project.org) a la configuración de Maven. Esta configuración se debe indicar en el archivo , siguiendo [estas instrucciones](preparacion-de-un-entorno-de-desarrollo-java-configuracion-de-maven.html)

El contenido de este archivo debería quedar así:

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
           <url>http://uqbar-wiki.org/mvn/releases</url>
         </repository>
         <repository>
           <snapshots/>
           <id>uqbar-wiki.org-snapshots</id>
           <name>uqbar-wiki.org-snapshots</name>
           <url>http://uqbar-wiki.org/mvn/snapshots</url>
         </repository>
       </repositories>
     </profile>
   </profiles>
   <activeProfiles>
     <activeProfile>uqbar-wiki</activeProfile>
   </activeProfiles>
 </settings>
```
