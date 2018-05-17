---
layout: article
title: Definiciones locales  where 
---

Hay ocasiones en las cuales nuestras funciones tienen una complejidad que puede disminuirse definiendo funciones más chicas que la componen, pero las mismas no son de particular interés fuera del contexto de esa función. Una forma de definir funciones que sólo se encuentran definidas en el contexto de una función es usando *where*.

Supongamos que queremos hacer una función que nos dice el estado de gordura de una persona y sabemos que esto se determina a partir de la altura y el peso de la misma

```Haskell
gordura peso altura
  | peso / altura ^ 2 <= 18.5 = "Desnutrido"  
  | peso / altura ^ 2 <= 25.0 = "Normal"  
  | peso / altura ^ 2 <= 30.0 = "Gordito"  
  | otherwise                 = "Obeso"  
```

Podemos ver que la expresión peso / altura ^ 2 se repite y sería bueno extraerlo a una función, pero para el resto del programa del médico clínico no tiene ninguna utilidad. Una forma elegante para definir la función gordura es mediante definiciones locales dándole un nombre a esta expresión.

```Haskell
gordura peso altura
  | indiceGordura <= 18.5 = "Desnutrido"  
  | indiceGordura <= 25.0 = "Normal"  
  | indiceGordura <= 30.0 = "Gordito"  
  | otherwise             = "Obeso"  
     where indiceGordura = peso / altura ^ 2
```

Algo simpático, como se ve en este ejemplo, es que podemos usar los argumentos de la función global en la local como si fueran constantes. Si quisiéramos poder variar los valores también es posible parametrizar a la función local como cualquier otra función.

```Haskell
holaDones don1 don2 = saludar don1 ++ saludar don2
   where saludar alguien = "Hola Don " ++ alguien ++ "! "
```

**Nota: Al igual que con las guardas, para que el where funcione hay que dejar al menos un espacio de indentación**
