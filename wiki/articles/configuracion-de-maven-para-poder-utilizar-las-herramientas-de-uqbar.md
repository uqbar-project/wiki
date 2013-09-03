A la configuración original de Maven debemos indicarle cuáles son los repositorios de donde bajar los artefactos de Uqbar. Para poder realizar esto es necesario agregar algunas indicaciones en el archivo ~/.m2/settings.xml (o crearlo en caso de que no exista).

Este archivo se encuentra en el directorio del usuario, esta ruta cambia dependiendo de cada sistema operativo. Por ejemplo en Linux, si el usuario es pablo, la ruta sería /home/pablo/.m2/settings.xml. Si trabajan con Windows, instalado en el drive C: y el nombre del usuario es Fernando, el directorio será C:\\Users\\Fernando\\.m2\\settings.xml

El contenido de este archivo debería quedar así:

` `<settings xmlns="http://maven.apache.org/POM/4.0.0"  
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/settings-1.0.0.xsd">
`   `<profiles>
`     `<profile>
`       `<id>`uqbar-wiki`</id>
`       `<repositories>
`         `<repository>
`           `<id>`uqbar-wiki.org-releases`</id>
`           `<name>`uqbar-wiki.org-releases`</name>
`           `<url>[`http://uqbar-wiki.org/mvn/releases`](http://uqbar-wiki.org/mvn/releases)</url>
`         `</repository>
`         `<repository>
`           `<snapshots/>
`           `<id>`uqbar-wiki.org-snapshots`</id>
`           `<name>`uqbar-wiki.org-snapshots`</name>
`           `<url>[`http://uqbar-wiki.org/mvn/snapshots`](http://uqbar-wiki.org/mvn/snapshots)</url>
`         `</repository>
`       `</repositories>
`     `</profile>
`   `</profiles>
`   `<activeProfiles>
`     `<activeProfile>`uqbar-wiki`</activeProfile>
`   `</activeProfiles>
` `</settings>
