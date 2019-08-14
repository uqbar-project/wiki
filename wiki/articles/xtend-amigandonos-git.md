---
layout: article
title: Xtend - control de versiones
categories: [xtend, lenguaje, git, equipo, trabajo, versionado, control, VCS]
featured: true
---


No todos los archivos deben subirse al repo. Como regla general no deberían subir archivos que se puedan generar a partir de otros, por ejemplo:

- los archivos `.java` que se generan a partir del código Xtend (en `target/xtend-gen/main`)
- los binarios que se generan a partir del código fuente de ustedes (en `target/classes`). Ocupan espacio en el repositorio y se corre el riesgo de estar trabajando con versiones desactualizadas.
- archivos de configuración propios de Eclipse, como `.project`, `.classpath` y el directorio `.settings`. Todos estos se generan a partir del `pom.xml` cuando hacemos Maven > Update, y por este motivo no es bueno trabajar con los archivos de Eclipse ni tenerlos en cuenta en el control de versiones.

# Cómo no subir esos archivos

Debemos crear un archivo `.gitignore` (que en Wollok se los creó el propio entorno), en la carpeta raíz del proyecto, que debe tener al menos esta lista:

```bash
/target/
.classpath
.project
bin
generated*
.settings
```

Fíjense que hemos eliminado todos los archivos que se pueden generar en base a definiciones originales. Esto da como resultado un repositorio con menos cantidad de archivos y mayor calidad de los mismos. Por lo general, deberíamos evitar subir archivos binarios (salvo imágenes o archivos encriptados con información sensible), dado que ocupan más espacio y no se puede comparar diferencias entre versiones, solo saber si cambió.

# Recomendaciones para trabajar con mi grupo

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

# Metodología para trabajar en grupo

- los tests tienen que estar en verde **siempre**
- los tests son de todos y todos somos responsables por mantenerlos
- si encontramos un bug y no había un test que lo probaba agregamos uno
- los tests son rápidos de correr (no pueden tardar 10 minutos)

# Git por consola o Git con una herramienta visual?

Da lo mismo, elegí la herramienta que mejor te resulte. eGit no es la gran cosa y hay otras opciones (tenés los links en la página), lo importante es cómo te organizás con tus compañeros.

___

- [Siguiente paso: Guía rápida de Xtend](xtend-guia-rapida.html)
- [Volver al menú principal del entorno Xtend](xtend-principal.html)
