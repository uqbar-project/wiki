---
layout: article
title: Igual o identico     vs   
---

Definiciones
------------

**Identidad:** decimos que dos objetos son idénticos si son el mismo objeto. Dentro del ambiente podemos tener dos referencias diferentes al mismo objeto. En Wollok el operador usado para comparar dos objetos por identidad es `===`.

**Igualdad:** (o **equivalencia**) *por defecto* dos objetos son iguales si son idénticos, o sea si son el mismo objeto. A diferencia de la identidad, la igualdad puede ser [redefinida](redefinicion.html) con una implementación específica si el problema lo amerita. En Wollok el operador usado para comparar dos objetos por igualdad es `==`.

Ejemplos
--------

Algunos objetos literales como los números o los booleanos son únicos en el sistema, por ese motivo evaluar `3 === 3` da true al igual que `3 == 3`.

Sin embargo, lo normal es hacer las comparaciones con `==` y no con `===`, ya que en general lo que interesa saber es si dos objetos son iguales desde el punto de vista de dominio. Un ejemplo muy común son los strings: supongamos que queremos saber si el resultado de concatenar "hola" con "mundo" es el string "holamundo". No es relevante si el resultado de esa concatenación fue exactamente el mismo objeto que se puede obtener al escribir el literal "holamundo", de hecho, es normal que no lo sea:

```wollok
> "hola" + "mundo" == "holamundo"
=> true
> "hola" + "mundo" === "holamundo"
=> false
```

Dando un paso más, ¿en qué casos podría ser útil redefinir la igualdad? Hay algunos objetos que tienen como principal objetivo representar datos más allá de los literales que ya conocemos, como ser las fechas. Si yo evaluamos esto en una consola:

```wollok
> const fecha1 = new Date(day = 24, month = 11, year = 2017)
> const fecha2 = new Date(day = 24, month = 11, year = 2017)
```

Luego podemos confirmar que la igualdad para las fechas está definida en términos de los datos, la identidad no es relevante:

```wollok
> fecha1 == fecha2
=> true
> fecha1 === fecha2
=> false
```

**Importante** Sólo debería redefinirse la igualdad basado en valores que no vayan a cambiar. Por ejemplo si quisiéramos modelar una dirección que tiene como atributos la calle y la numeración y nos interesara definir la igualdad en esos términos, es importante que una vez construido el objeto dirección con la calle y la numeración, no sea posible cambiar esas referencias, ya que la relación de igualdad entre dos objetos debería mantenerse a lo largo del tiempo. O sea, el objeto no necesita ser totalmente [inmutable](inmutabilidad.html) pero sí debemos garantizar que mínimamente lo sea respecto a los valores usados para la igualdad.

Otra cosa a tener en cuenta si se redefine la igualdad es que existen otros objetos del sistema (como las colecciones) que se basan en otro mensaje (suele llamarse *hash* o *hashCode*) para encontrar rápidamente objetos que son (o podrían ser) iguales al receptor de dicho mensaje, es común que se use para algoritmos de búsqueda. Su particularidad es que, en vez de recibir otro objeto por parámetro para poder compararlo, no recibe parámetros y retorna un número al que se denomina *código de hash*. En general si la igualdad se define de una forma particular y se mantiene la forma de calcular el código de hash predefinida, eventualmente surgirán comportamientos inesperados. Por eso, independientemente de la tecnología que uses, asegurate de averiguar cúal es la forma más adecuada de implementar ese mensaje.

En el caso del ejemplo planteado de las direcciones, podría ser razonable que la igualdad se defina preguntando si la calle de la dirección que recibió el mensaje es igual a la calle de la otra dirección y además la numeración de esa dirección también es igual a la numeración de la otra dirección. Luego el código de hash podría obtenerse mediante un cálculo que involucre tanto al código de hash de la calle de la dirección que recibió el mensaje y también al código de hash de su numeración.
