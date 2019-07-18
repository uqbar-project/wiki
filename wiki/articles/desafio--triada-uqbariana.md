---
layout: article
title: Desafio  triada uqbariana
---

Llamamos "triada" a cualquier conjunto de 3 números `{ x; y; z }`. En particular nos enfocaremos en las _triadas uniformes_ que son cualquier conjunto de 3 números _iguales_ `{ x; x; x }`.

Decimos que una triada se **aparenta** a un número `N` si existe alguna forma de operar los tres números de la triada de modo que el resultado sea `N`.
Por ejemplo, 
- la triada `{ 2; 2; 2 }` se aparenta al `3` porque `2 + 2 / 2 = 3`
- la triada `{ 1; 1; 1 }` se aparenta al `2` porque `(1 + 1) * 1 = 2`

Las restricciones para operar la triada son:
1. No se permite agregar más números
2. Solamente usaremos las operaciones básicas: `{ +; -; *; /; }`, ninguna más


Por último, definiremos las _triadas uqbarianas_ como todas las triadas uniformes que van del 1 al 9, o sea:
```
{ 1; 1; 1 }
{ 2; 2; 2 }
{ 3; 3; 3 }
{ 4; 4; 4 }
{ 5; 5; 5 }
{ 6; 6; 6 }
{ 7; 7; 7 }
{ 8; 8; 8 }
{ 9; 9; 9 }
```

### Parte 1

La primera parte del desafío consiste en encontrar algún _número uqbariano_, sabiendo que éste 
- Es un número entero positivo, y
- Se aparenta con todas las triadas uqbarianas

**Se deberá indicar cuál es el número uqbariano y dar algún ejemplo de cómo se aparenta con cada triada uqbariana.**


### Parte 2

Así como existen las triadas, también existen los **cuartetos**, que son conjuntos de cuatro números. Y análogamente existen los _cuartetos uqbarianos_ que son todos los cuartetos uniformes que van del 1 al 9.

**Indicar _todos_ los números uqbarianos para los cuartetos.**


### Parte 3

Todos estos conjuntos se agrupan dentro de lo que llamamos **familias**, que se identifican con un _rango_ siendo éste la cantidad de elementos dentro de un conjunto. Así, por ejemplo:
- La familia uqbariana de rango 3 es equivalente a las triadas uqbarianas.
- La familia uqbariana de rango 4 es equivalente a los cuartetos uqbarianos.

Además, decimos que una familia es _compacta_ cuando se aparenta con todos los números del 1 al 9.

**Determinar cuál es el rango de la primera familia uqbariana compacta.**


### Parte BONUS

Este ejercicio fue inspirado en el problema de [este video](https://www.youtube.com/watch?v=h2vkrxvh76c). 

**Para llegar a sus mismos resultados deberíamos agregar al programa las operaciones: _raíz cuadrada_ y _factorial_.**

-----
### Ayudas

1. En Prolog las operaciones matemáticas ¡son functores! Así que podemos relacionarlas como cualquier otro individuo. Por ej:
```Prolog
areaCirculo(Radio, pi * Radio ^ 2). % Se relaciona un radio con la FÓRMULA del área
```
Se puede hacer la consulta
```Prolog
?- areaCirculo(2, Formula), Area is Formula.
Formula = pi*2^2,
Area = 12.566370614359172.
```

2. OJO con Integers vs Floats, porque no son iguales:
```Prolog
?- 0.0 is 1 - 1.
false.
```
Tal vez quieras comparar todos los resultados con su representación flotante:
```Prolog
?- 0.0 is float(1 - 1).
true.
```
