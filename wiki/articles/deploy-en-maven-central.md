Estás desarrollando un proyecto Maven para Uqbar - el cual tiene como pom padre uqbar-parent-project - y ahora lo querés desplegar en Maven Central, ¿que hacer?

1. Creamos el release

`$ mvn --batch-mode release:clean release:prepare`

Este comando crea un commit y un tag para la nueva versión y luego otro commit agregandole nuevamente el -SNAPSHOT a la versión. La opción *--batch-mode* hace que el plugin no pregunte la versión y asuma la correlativa a la actual, puede omitirse si se quiere especificar otra versión.

2. Verificamos que todo esté bien

`$ git log # deberia haber dos commits nuevos`
`$ git tag -l # deberia haber un tag nuevo`

3. Pusheamos los 2 commits nuevos y el tag

`$ git push --follow-tags`

4. Nos paramos en el tag que acabamos de crear

` $ git checkout [nombre del release, por ejemplo v3.3]`

5. Publicamos el release en Sonatype

` $ mvn clean deploy -Prelease # {poner la pwd de OSS: }`

6. (Opcional) Volvemos a master para seguir trabajando

` $ git checkout master`
