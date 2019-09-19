---
layout: article
title: Manejo de dependencias con NPM
featured: true
---

# Versionado semántico

Recordemos que todo componente tiene una versión compuesta por tres números:

- major
- minor
- patch

Ejemplo: el componente "typescript" tiene la versión "3.6.3", donde 3 es el major number, 6 el minor y finalmente 3 es el patch.

Para respetar el versionado semántico, al liberar una nueva versión tenemos que indicar qué es lo que trae ese nuevo componente:

- si soluciona _bugs_, debería incrementar el patch: el componente "typescript" pasaría a tener la versión "3.6.4"
- si trae nuevos _features_ o funcionalidades que **no rompen la retrocompatibilidad**, debería incrementar el minor a "3.7.0"
- si trae nuevas funcionalidades que rompen la retrocompatibilidad (ej. métodos o clases que dejaron de existir, o tienen diferente cantidad de parámetros), deberíamos incrementar la major a "4.0.0"

# Dependencias de un proyecto con NPM

## package.json

El Node Package Manager (NPM) almacena las dependencias de nuestro proyecto (entre otras configuraciones) en el archivo `package.json`, por ejemplo:

```json
  "dependencies": {
    "@angular/animations": "^8.2.7",
    "@angular/common": "^8.2.7",
    "@angular/compiler": "^8.2.7",
    "@angular/core": "^8.2.7",
  },
  "devDependencies": {
    "@angular-devkit/build-angular": "^0.803.5",
    "@angular/cli": "~8.3.5",
    "@angular/compiler-cli": "^8.2.7",
    "@angular/language-service": "^8.2.7",
    "typescript": "~3.6.3"
  }
```

Por ejemplo, "~3.6.3" implica que podemos subir hasta una versión minor (3.7.x), pero no más (no a la 3.8.1 por ejemplo). El modificador "^8.2.7", implica que podemos subir todas las versiones minor y patch que necesitemos, pero no actualizaremos la versión major a "9.0.0" por ejemplo.

Para más información pueden leer [el componente de node que trabaja el _versionado semántico_](https://docs.npmjs.com/misc/semver). 

## package-lock.json

Mientras que el `package.json` permite configurar las dependencias para un rango de versiones posibles, el `package-lock.json` es la foto de las dependencias exactas que se descargaron en tu máquina, incluyendo las dependencias indirectas.

Esto permite que todos los desarrolladores (y especialmente el CI) puedan replicar el mismo entorno y que las versiones que instalamos no dependa del momento en el que hacemos `npm install`.

# Primera instalación con npm install

La primera vez que descargamos un proyecto, debemos descargar las dependencias, para eso debemos ejecutar

```bash
npm install
```

Si existe el archivo `package-lock.json`, npm partirá de esa definición para ir a buscar las dependencias y bajarlas a nuestro directorio node_modules. Si no existe el archivo `package-lock.json`, entonces se basará en las definiciones del `package.json` para buscar las últimas versiones posibles de cada uno de los componentes.

# ¿Tengo que versionar el package-json? (subirlo a git)

TL;DR **Sí**.

La justificación es que de esa manera estaremos seguros de que todos los que trabajamos en el proyecto tengamos las mismas dependencias.

# Actualizando nuestras dependencias

Para mantener al día las dependencias de nuestros ejemplos, tenemos dos opciones:

## Subir las versiones minor - npm update

Podemos detectar si hay nuevas versiones _minor_ que no rompan la retrocompatibilidad escribiendo en nuestra consola o git bash:

```bash
npm outdated
```

O directamente podemos escribir:

```bash
npm update
```

Al ejecutar este comando:

- se actualiza el `package-lock.json`
- y también en el directorio `node_modules` descargamos los paquetes con el código que vamos a utilizar

![node_modules_meme](/img/wiki/node_modules_meme.png)

## Subir las versiones major (con coraje)

Si estás seguro que vas a poder seguir manteniendo estable tu conjunto de dependencias, podés trabajar con npm-check-updates. Lo instalás globalmente la primera vez:

```bash
npm install -g npm-check-updates
```

Y luego lo ejecutás

```bash
ncu -u
```

Esto **te va actualizar las versiones de tu `package.json` tratando de subir a la mayor versión posible**. Luego podés correr `npm update` (o bien `npm install` si te bajaste el proyecto la primera vez)
