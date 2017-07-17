---
layout: article
title: Deploy en maven central
---

Estás desarrollando un proyecto Maven para Uqbar -el cual tiene como pom padre *uqbar-parent-project*- y ahora lo querés desplegar en Maven Central, ¿que hacer?

Tenemos 2 tareas para hacer: **preparar** el release y luego **publicarlo**. La primera sí o sí vas a tener que hacerla en tu PC, porque vos sos responsable de, por ejemplo, elegir la versión de este nuevo release. Veamos entonces cómo hacer esto:

Preparar el release
-------------------

1. Creamos el release

`$ mvn --batch-mode release:clean release:prepare`

Este comando crea un commit y un tag para la nueva versión y luego otro commit agregandole nuevamente el -SNAPSHOT a la versión. La opción *--batch-mode* hace que el plugin no pregunte la versión y asuma la correlativa a la actual, puede omitirse si se quiere especificar otra versión (siempre siguiendo los lineamientos planteados por [Semantic Versioning](http://semver.org/)).

2. Verificamos que todo esté bien

`$ git log # deberia haber dos commits nuevos`
`$ git tag -l # deberia haber un tag nuevo`
`$ git status # no debería quedar nada sin commitear`

Si quedaron cambios sin commitear podés agregarlos al último commit

`$ git commit --amend --no-edit`

3. Pusheamos los 2 commits nuevos y el tag

`$ git push --follow-tags`

Y con esto ya tendremos en nuestro repositorio GitHub todo lo necesario para publicar el nuevo release.

Publicar el release
-------------------

Este punto va a depender de cómo esté configurado el repositorio. Si bien configurada la integración continua via [Travis](https://travis-ci.org/), entonces no tenés que hacer más nada. Por las dudas, entrá a Travis y fijate que se esté buildeando el tag que acabás de subir (esto podés verlo en la parte de *Branches*).

Si no tiene configurado Travis, entonces tenés dos opciones:

- Configurarlo vos :). No es difícil y tenemos una guía de cómo hacerlo en el blog de Uqbar (próximamente...).

- Hacerlo a mano. Tené en cuenta que para esto tenés que tener una cuenta en Sonatype autorizada para subir artefactos de Uqbar y toda la configuración necesaria para que funcione ([firma GPG publicada](http://central.sonatype.org/pages/working-with-pgp-signatures.html), credenciales en el *settings.xml*, etc), lo cual te va a llevar más tiempo que configurar el CI.

Si igualmente querés hacerlo, los pasos son estos:

1. Nos paramos en el tag que acabamos de crear

` $ git checkout [nombre del release, por ejemplo v3.3]`

2. Publicamos el release en Sonatype

` $ mvn clean deploy -Prelease # {poner la pwd de OSS: }`

3. (Opcional) Volvemos a master para seguir trabajando

` $ git checkout master`
