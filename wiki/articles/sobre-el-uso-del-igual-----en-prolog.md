---
layout: article
title: Sobre el uso del igual en prolog
---

### Descripción del problema

Vamos a estudiar éste caso:

-  "Dos personas son hermanastros cuando tienen igual padre y diferente madre"

### El is sólo se usa para cuentas, las cuentas sólo se hacen con is

Ésta solución funciona, pero es **conceptualmente errónea**:

```Prolog
hermanastros(Hermano1,Hermano2):-
   padre(Hermano1,Padre1),
   padre(Hermano2,Padre2),
   madre(Hermano1,Madre1),
   madre(Hermano2,Madre2),
   Padre1 is Padre2,  %%% Acá está el error
   Madre1 \= Madre2.
```

**Si el is sólo sirve para cuentas,**

-   No es conceptualmente correcto hacer `<algo> is Variable.`
-   No es conceptualmente correcto hacer `<algo> is 4.`

Si bien funciona, debemos usar las herramientas para lo que corresponde. Cuando una persona lee un is, espera cuentas. Debemos usar las abstracciones que comuniquen correctamente nuestras ideas.

**Si las cuentas sólo se hacen con is,**

-  Esto ni siquiera funciona: `Variable = 5 * 3.`.
-  Esto ni siquiera funciona: `factorial(N+1,Fac).` (¡Hay una cuenta, y falta un is!)

La razón por la que `Variable = 5 * 3.` no funciona es que el `=` es "tonto", se fija que a izquierda y a derecha haya _exactamente lo mismo_. Ver [aritmética en prolog](aritmetica-en-prolog.html).

La razón por la que `factorial(N+1,Fac).` no funciona es que la unificación que realiza prolog de valor con variable es igual de "tonta" que el `=`. Salvo que yo ponga un `is`, no se va a resolver ninguna cuenta.

**Entonces:**

-   **Sí** vale `Resultado is Variable + 1.` (Siempre que `Variable` venga unificada).

Más abajo la solución certera. 

### Variable = OtraVariable

Ésta solución funciona, pero es **conceptualmente errónea**:

```Prolog
hermanastros(Hermano1,Hermano2):-
   padre(Hermano1,Padre1),
   padre(Hermano2,Padre2),
   madre(Hermano1,Madre1),
   madre(Hermano2,Madre2),
   Padre1 = Padre2,  %%% Acá está el error
   Madre1 \= Madre2.
```

**¿Por qué está mal?** Porque tiene cierta imperatividad, en donde nosotros estamos forzando a mano que los padres sean iguales, y que las madres sean diferentes.

La idea de *verificación* que nos ofrece Lógico nos permite representar una igualdad de manera mucho más sencilla, más directa. Sabiendo que Prolog *considera cierta una consulta si sus variables matchean, y falsas si no*, podemos hacer las cosas más declarativas:

Si el padre *es el mismo*, entonces, *que sea la misma variable*. Éste código funciona, y es **conceptualmente correcto**:

```Prolog
hermanastros(Hermano1,Hermano2):-
   padre(Hermano1,Padre), %%% Usamos variable Padre
   padre(Hermano2,Padre), %%% Usamos misma variable Padre
   madre(Hermano1,Madre1),
   madre(Hermano2,Madre2),
   Madre1 \= Madre2.
```

Así se aprovechan mejor las herramientas mencionadas, y redujimos un poco la imperatividad.

*Que dos variables tengan el mismo nombre, es una restricción implícita de igualdad*

Por otro lado, en el caso de las variables `Madre1` y `Madre2`, como *las variables con nombres diferentes no representan una restricción de diferencia*, tenemos que forzar dicha restricción con el `\=`.

### Variable = individuo

Tomemos el siguiente problema: "Un instrumento suena lindo si es de cuerda y está afinado". 

Solución:

```Prolog
suenaLindo(Instrumento):-
    tipo(Instrumento,Tipo), %%% Pido el tipo
    Tipo = cuerda,          %%% comparo con cuerda.
    estaAfinado(Instrumento).
```

Eso funciona, pero está **conceptualmente mal**.

¿Por qué? Veamos:

-   **La unificación de una variable se da una sola vez en la ejecución.**

Eso significa que, en un programa en lógico, los valores de las variables no cambian con el tiempo (una vez unificadas). En nuestro caso, una vez unificado con `cuerda`, la variable `Tipo` siempre es `cuerda`.

-   **La unificación se da para todas las ocurrencias de la misma, e instantáneamente**

Eso significa que, en un programa en lógico, la variable "vale" lo mismo en cualquier parte de la regla. En nuestro caso, al unificarse `Tipo` con algún tipo en la 2da línea, eso "llena" la variable automáticamente en la 3era línea. ¡Y no puede cambiar el valor!

En consecuencia, decir `Tipo = cuerda` es *exactamente lo mismo* que escribir `cuerda` en todos los lugares donde escribimos `Tipo`. La solución correcta:

```Prolog
suenaLindo(Instrumento):-
    tipo(Instrumento,`**`cuerda`**`),
    estaAfinado(Instrumento).
```

Es muy común ver este tipo de errores en predicados polimórficos y con functores. Ésto funciona, pero está conceptualmente mal:

```Prolog
potencia(Habilidad,Potencia):-
     Habilidad = velocista(VelMax),
     Potencia = VelMax.
```

Ésta es la forma **correcta**:

```Prolog
potencia(velocista(VelMax),VelMax).
```

### Conclusión

El igual es necesario en contados casos. La mayoría de las veces uno se puede arreglar con la metáfora de identidad de lógico, y con un poquito de unificación y pattern matching.

Usemos con criterio las herramientas y conceptos que nos da el paradigma.
