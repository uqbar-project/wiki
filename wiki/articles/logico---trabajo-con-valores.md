Cuándo se usa **is**
--------------------

Cuando tengo que hacer una operación aritmética, p.ej..

`   ingresoTotal(Pers,IT):- ingresoPorSueldo(Pers,IS), ingresoPorRenta(Pers,IR), IT is IS+IR.`

en este caso, la cuenta es una suma, debo usar is.

Si lo de la izquierda es un valor, o una variable ligada con un valor, entonces el is se verifica si el resultado de la cuenta es el valor. P.ej. si quiero saber si juan tiene exactamente el doble de ingreso que roque, puedo hacer esta consulta

`   ?- ingresoTotal(juan,IngrJuan), ingresoTotal(roque,IngrRoque), IngrJuan is IngrRoque * 2.`

No está bien usar el is cuando lo de la derecha no es una cuenta. Más detalles a continuación.

Si sé qué valor es, pongo el valor
----------------------------------

Si en un programa quiero representar "bart es hijo de homero", ¿pongo?

`  hijo(X,Y):- X=bart,Y=homero.`

o pongo

`  hijo(bart,homero).`

Está claro que pongo lo segundo. ¿Por qué? Porque no quiero decir "X e Y son hijo y padre si a X le pasa tal cosa y a Y le pasa tal otra", sino que ya sé quiénes son los que quiero relacionar, bart y homero.

En todos (**todos**) los casos en los que ya sé el valor, lo correcto es poner el valor, no hace falta "pasar" por una variable (como sería la variable X para bart en el ejemplo de arriba). Algunos ejemplos menos obvios.

1.  
Estoy haciendo un programa que modela gustos en propiedades de la gente, donde a cada propiedad la modelo con un átomo que describe su dirección (p.ej. `lavalle851`). Tengo estas reglas

-   A los que viven en Belgrano le gustan los departamentos lujosos.
-   A Roque le gustan los departamentos chiquitos.
-   A los que viven en Boedo les gusta el departamento de Corrientes 3804.

<!-- -->

  
En la primera regla sólo me dan condiciones sobre persona y depto:

`  gusta(Pers,Depto):- vive(Pers,belgrano), esLujoso(Depto).`

  
En la segunda, no me están hablando de una persona cualquiera, me están hablando de Roque. Entonces el primer argumento no necesita ser una variable

`  gusta(roque,Depto):- esChiquito(Depto).`

  
Para la tercera, del que me están hablando específicamente es del depto, de la persona me dan condiciones. Entonces

`  gusta(Pers,corrientes3804):- vive(Pers,boedo).`

2.  
En el mismo programa, quiero decir que un departamento es alternativa para otro si sale exactamente 5000 pesos menos. En este caso tengo que hacer una cuenta (is), perolo que ya sé es la difere


