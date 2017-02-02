---
layout: article
title: Hugs trex insertfield not in scope
---

Si están en Hugs y luego de cargar su programa aparece el siguiente error:

Haskell 98 does not support extensible records ERROR - Hugs.Trex.insertField not in scope

-   -   Possible cause: "Hugs.Trex" module not imported

Revisen que en sus funciones no estén usando = para comparar dentro de la definición de una función. Para comparaciones por igualdad se usa ==
