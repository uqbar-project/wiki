Si sé qué valor es, pongo el valor
----------------------------------

Si en un programa quiero representar "bart es hijo de homero", ¿pongo?

`  hijo(X,Y):- X=bart,Y=homero.`

o pongo

`  hijo(bart,homero).`

Está claro que pongo lo segundo. ¿Por qué? Porque no quiero decir "X e Y son hijo y padre si a X le pasa tal cosa y a Y le pasa tal otra", sino que ya sé quiénes son los que quiero relacionar, bart y homero.

En todos (**todos**) los casos en los que ya sé el valor, lo correcto es poner el valor, no hace falta "pasar" por una variable (como sería la variable X para bart en el ejemplo de arriba). Algunos ejemplos menos obvios.

1. Estoy haciendo un programa que modela gustos en propiedades de la gente, donde a cada propiedad la modelo con un átomo que describe su dirección (p.ej. `lavalle851`). Tengo estas reglas

-   A los que viven en Belgrano le gustan los departamentos lujosos.
-   A Roque le gustan los departamentos chiquitos.
-   A los que viven en Boedo les gusta el departamento de Corrientes 3804.

En la primera regla sólo me dan condiciones sobre persona y depto:

`  gusta(Pers,Depto):- vive(Pers,belgrano), esLujoso(Depto).`

En la segunda, no me están hablando de una persona cualquiera, me están hablando de Roque. Entonces el primer argumento no necesita ser una variable

`  gusta(roque,Depto):- esChiquito(Depto).`

Para la tercera, del que me están hablando específicamente es del depto, de la persona me dan condiciones. Entonces

`  gusta(Pers,corrientes3804):- vive(Pers,boedo).`
