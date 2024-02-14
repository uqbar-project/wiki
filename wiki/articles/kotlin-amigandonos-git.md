---
layout: article
title: Kotlin - control de versiones
categories: [kotlin, lenguaje, git, equipo, trabajo, versionado, control, VCS]
featured: true
---

# Los archivos a versionar en Git

No todos los archivos deben subirse al repo. Como regla general no deberían subir archivos que se puedan generar a partir de otros, por ejemplo:

- los binarios que se generan a partir del código fuente de ustedes (el directorio `build`). Ocupan espacio en el repositorio y se corre el riesgo de estar trabajando con versiones desactualizadas.
- archivos de configuración propios de IntelliJ (el directorio `.idea`), que se pueden armar en base al `build.gradle.kts` cuando se importa el proyecto en nuestra máquina la primera vez.
- archivos que genera _Gradle_ localmente (el directorio `.gradle`, distinto al `gradle` que es donde está el script ejecutable o [wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html)). Si querés conocer más en profundidad la estructura de las carpetas que utiliza Gradle, te recomendamos [esta página](https://docs.gradle.org/current/userguide/directory_layout.html).

## Archivos a ignorar

Debemos crear un archivo `.gitignore` (que en Wollok se los creó el propio entorno), en la carpeta raíz del proyecto, que debe tener al menos esta lista:

```bash
HELP.md
.gradle
build/
!gradle/wrapper/gradle-wrapper.jar
!**/src/main/**/build/
!**/src/test/**/build/

### STS ###
.apt_generated
.classpath
.factorypath
.project
.settings
.springBeans
.sts4-cache
bin/
!**/src/main/**/bin/
!**/src/test/**/bin/

### IntelliJ IDEA ###
.idea
*.iws
*.iml
*.ipr
out/
!**/src/main/**/out/
!**/src/test/**/out/

### NetBeans ###
/nbproject/private/
/nbbuild/
/dist/
/nbdist/
/.nb-gradle/

### VS Code ###
.vscode/
```

## Git por consola o Git con una herramienta visual?

Da lo mismo, elegí la herramienta que mejor te resulte. El plugin de IntelliJ tiene un buen soporte para Git, aun así hay otras opciones (tenés los links en la página), lo importante es cómo te organizás con tus compañeres.


# Recomendaciones para trabajar con mi grupo

## Un día en la vida de una persona que desarrolla (si van a trabajar en una rama sola)

- Cuando empiezo el día primero sincronizo el repositorio para ver los cambios que no tengo en el código. Si no hay cambios, simplemente corro los tests y empiezo a codear como un campeón. Si no...
  - Acepto los cambios entrantes y en caso de ser necesario resuelvo conflictos
  - Corro los tests y veo que todo anda sobre ruedas
  - Vuelvo a sincronizar y veo que ya no quedan ni conflictos ni cambios sin aceptar
  - Subo mis cambios al repositorio remoto para que mis compañeros lo vean
- Ahora sí, a programo, programo, programo... y cuando termino, corro los tests
- Y vuelvo a sincronizar contra el repositorio remoto

En resumen:

- No pasa un día de trabajo sin hacer un commit y push al repositorio remoto. Esto implica planificar mi trabajo para que pueda subir algo al repositorio sin que rompa todo: **hay que partir un cambio grande en pequeños pasos** (iterativo, incremental).
- Nunca deberían subir nuevos fuentes al repositorio sin explicar brevemente qué cambiaron. Si los mensajes son descriptivos (y "fix", "asdsadsa" o "arreglo una cosita" definitivamente no lo son) rápidamente puedo detectar qué modificaron mis compañeros con sólo leer lo que escribieron en los commits. Una buena descripción me ayuda también a entender qué es lo que se modificó y por qué razón, especialmente útil a la hora de solucionar un conflicto o entender por qué se rompieron los tests.
- El que rompe los tests paga las facturas.

## ¿Y si cada uno trabaja en una [rama separada](https://git-scm.com/book/es/v2/Ramificaciones-en-Git-Procedimientos-B%C3%A1sicos-para-Ramificar-y-Fusionar)?

- "mejor, no voy a tener conflictos": eso no es cierto. Cada vez que integres tu código en el branch principal se pueden romper tests o funcionalidades
- no puede pasar más de 3 días sin abrir el PR (Pull Request), dejar pasar el tiempo hace que integrar tu código sea un proceso largo y tedioso
- un TP no se puede entregar en 5 ramas distintas, tiene que haber una sola fuente de verdad y es la rama principal del repositorio remoto
- tener integración continua es fundamental si queremos trabajar con ramas

## Metodología para trabajar en grupo

- los tests tienen que estar en verde **siempre**
- los tests son de todos y todos somos responsables por mantenerlos
- si encontramos un bug y no había un test que lo probaba agregamos uno
- los tests son rápidos de correr (no pueden tardar 10 minutos)

<!-- -->

# Links útiles

- [Siguiente paso: Guía rápida de Kotlin](kotlin-guia-rapida.html)
- [Volver al menú principal del entorno Kotlin](kotlin-principal.html)
