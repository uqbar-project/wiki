# Uqbar wiki

[![CircleCI](https://circleci.com/gh/uqbar-project/wiki.svg?style=svg)](https://circleci.com/gh/uqbar-project/wiki)

Wiki armada en base a wiki.template de jekyll con algunos plugins, como git metadata, categorías, custom tags, etc.

Se puede ver todo lo que ofrece la wiki en terminos de implementación y features en este [link](https://uqbar-project.github.io/wiki/wiki/articles/como-empezar.html)


## Instalacion local

### Requerimientos

Para testear localmente se necesitan los siguientes requerimientos previos.

- Ruby 2.5.0
- Bundler
- imagemagick
- yarn
- python

### Instalando y levantando la wiki localmente

Para instalar las dependencias de ruby adicionales ejecutar:

```bash
bundle install
```

Este proyecto utiliza Jekyll, para levantar una instancia local una vez instaladas las dependencias habrá que instalar las dependencias adicionales mediante yarn:

```bash
yarn install
```

Una vez ya resueltas todas las dependencias se puede levantar el entorno mediante:

```
jekyll s --i
```
    
Esto arma un build local y una vez completado este proceso levanta localmente la instancia. De aquí en más se podrá hacer modificaciones localmente, y el servidor actualizará los cambios automáticamente (es lo que hace el flag --i de incremental).

Tener en cuenta que ante cualquier modificación fuera de los artículos, como los plugins, se necesitará ejecutar un nuevo build de jekyll. (El servidor se levantará por default en localhost:4000)

## Rake tasks

Existen actualmente dos rake tasks 

### Generate

Esta rake task genera el build de los md, y genera en base a estos los html que servira despues jekyll.

### Test

Esta rake task verifica los links que no autoreferencien a otras paginas de la wiki, o sea, links externos. Se tiene que tener el build ya armado ejecutando el generate o bien levantando el servicio previamente.

### Publish

Esta rake task es tal vez la mas importante porque es la que usa el CI para publicar el build en gh-pages en el repo de github.

