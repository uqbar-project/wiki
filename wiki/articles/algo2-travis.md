---
layout: article
title: Travis para Algoritmos 2
categories: [travis, ci, integracion, continua]
featured: true
---

## Integración continua con Travis

### Configuración del archivo .travis.yml

Create un archivo `.travis.yml` en el directorio raíz del proyecto que tenga este contenido

```yml
language: java
jdk:
  - oraclejdk11

script:
  - mvn -e -DTRAVIS_JOB_ID=$TRAVIS_JOB_ID clean install jacoco:report coveralls:report

notifications:
  email:
    - integrante1@unsam.edu.ar
    - integrante2@unsam.edu.ar
```

Reemplazá `integranteX@unsam.edu.ar` por los mails de los integrantes del grupo e incluí a tu tutor. No pasa nada si recibe un mail, eso no influye en la nota. Posta. En serio.

Si querés hacer algunos ajustes extras en las notificaciones, podés buscar [la documentación oficial de Travis](https://docs.travis-ci.com/user/notifications/#Configuring-email-notifications).

### Qué pasa entonces

A partir de aquí, cada vez que hagas un push, [Travis](https://www.travis-ci.com/) como servidor de integración continua

- clonará el repositorio tuyo
- lo compilará en la tecnología en la que trabajaste
- ejecutará los tests
- y generará el nuevo status de tu proyecto
- en caso de error, te mandará un mail avisándote que el build falló
- y si anduvo ok, te dirá que el build pasó
- de aquí en más, cada vez que hagas un push al repositorio, si anda ok no te dirá nada, pero si falla, te mandará un mail. Es mejor saberlo antes, no?

### Agregando el Badge al README

El _badge_ es un indicador visual de cómo resultó el último build, que ubicaremos en el archivo README para rápidamente darnos cuenta de si anduvo todo bien. Para eso, ingresamos a Travis en https://www.travis-ci.com/, elegimos nuestro repositorio, a la derecha tenemos el badge con el status. Le damos click en la imagen y nos aparecerá una ventana, cambiamos la segunda opción para que aparezca `Markdown` y a continuación copiamos el texto que se visualiza abajo.

![](/img/wiki/travisStatusBadge2.gif)

Vamos al editor, pegamos el texto en el README y pusheamos al repositorio.
