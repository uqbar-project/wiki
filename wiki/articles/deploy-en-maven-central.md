Estás desarrollando un proyecto Maven para Uqbar - el cual tiene como pom padre uqbar-parent-project - y ahora lo querés desplegar en Maven Central, ¿que hacer?

1. Crear el release

`$ mvn release:prepare`

2. Verificar que todo este bien

`$ git log # deberia haber dos commits nuevos`
`$ git tag -l # deberia haber un tag nuevo`

3. Ahora hay que bajar a la copia local el release guardado en el tag dentro de git

` $ git checkout [nombre del release]`

4. Finalmente hacer el deploy del tag en sonatype

` $ mvn clean deploy -Prelease # {poner la pwd de OSS: }`
