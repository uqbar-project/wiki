---
layout: article
title: Instalacion de Entorno para HTML y CSS
featured: true
categories: [web, entorno, css, html, estático]
---

<img src="/img/wiki/css-logo.png" alt="CSS Logo" height="40%" width="40%">

# Entorno

Si estás en entorno Windows te recomendamos instalarte [Git Bash](https://gitforwindows.org/)

# Editor de Texto: Visual Studio Code

El editor de texto que vamos a soportar en la cursada es [**Visual Studio Code**](https://code.visualstudio.com/) (hay [una versión portable](https://sourceforge.net/projects/vscode-portable/) si estás en una máquina sin privilegios de administrador).

Dentro de Visual Studio Code, te recomendamos que crees un perfil vacío y lo asocies a tus nuevos proyectos Angular (podés ver [cómo se trabaja con perfiles en VSCode en este video](https://www.youtube.com/watch?v=_2F2Zt-_tUA). También te dejamos [este tutorial muy piola - en inglés](https://www.youtube.com/watch?v=QjvvqR9KyVo) y [la documentación oficial](https://code.visualstudio.com/docs/editor/profiles)).

## Instalación

### Importar extensiones

Podés importar [este archivo que trae todas las extensiones para Angular](./css.code-profile)

### Instalación manual

Si no podés instalar las extensiones del Visual Studio Code manualmente. Para 2025 son las siguientes

- **IntelliSense for CSS class names in HTML - Zignd**: te ofrece autocompletado para clases en HTML
- **CSS Peek - Pranay Prakash**: para poder encontrar las definiciones de estilos desde el HTML (con F12 - Go to definition)
- **HTMLHint - HTMLHint**: validación de sintaxis de HTML. Atención que la extensión HTMLHint de Mike Kaufman está deprecada, desde allí te redireccionan a la nueva.
- **Auto Rename Tag - Jun Han**: permite renombrar un tag tanto al abrir como al cerrar el tag.
- **Live Server (Five Server) - Yannick**: levanta un servidor local y refresca automáticamente el navegador ante cualquier cambio que hagas.
- **Image preview - Kiss Tamás**: muestra previsualizaciones de imágenes en el margen

y no te olvides de instalarte el **Git Extension Pack - Don Jayamanne** para poder integrarte con git

También es bueno chequear en la configuración (`Ctrl` + `,` o `Cmd` + `,` en Mac) que tengas activada la opción _Bracket Pair Colorization_.

![Bracket colorization VSC setting](/img/wiki/bracket-colorization.png)

# Alternativa a Visual Studio Code

## Web Storm

Otra opción es utilizar [Web Storm](https://www.jetbrains.com/webstorm/) (de la suite de IntelliJ), si tienen una cuenta de la facultad pueden solicitar una licencia educativa. Solo que como no vamos a aprovechar todas las herramientas de este IDE poderoso quizás convenga ir por el Visual Studio Code.

## Trabajo online

Si tenés ganas de practicar fuera del TP, podés trabajar directamente desde el navegador con CodePen, o bien probar Prepros

# Links relacionados

- [Temario Algoritmos III](algo3-temario.html)