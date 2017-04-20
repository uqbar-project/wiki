---
layout: article
title: Desafio  hacer que un data propio sea enum
---

Hacer que estas consultas funcionen exactamente así:

```Haskell
Main> [CPersona "a" 1 .. CPersona "c" 1]
[CPersona "a" 1, CPersona "b" 1, CPersona "c" 1]
Main> [CPersona "f" 1 .. CPersona "k" 1]
[CPersona "f" 1, CPersona "g" 1, CPersona "h" 1, CPersona "i" 1, CPersona "j" 1, CPersona "k" 1]
```
Pistas:

-   Hay que hacer que el tipo Persona pertenezca a la clase Enum.
-   Para ello hay que usar la instrucción "instance" y definir dos funciones.
-   [Acá hay un ejemplo](https://www.haskell.org/onlinereport/standard-prelude.html) de cómo están definidos los tipos básicos y sus typeclasses.

