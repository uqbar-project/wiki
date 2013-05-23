Los primeros individuos con los cuales estaremos trabajando son los denominados simples (en contraposición a los [individuos compuestos](paradigma-logico---individuos-compuestos.html)). Los que nos van a interesar para trabajar en general son los **átomos** y los **números**.

Átomos
------

Los átomos son valores que representan a una entidad indivisible. En el siguiente ejemplo podemos ver cómo se usan átomos para modelar la información:

`padre(homero,bart).`
`padre(homero,lisa).`
`padre(homero,maggie).`
`padre(abe,homero).`

Los átomos que se usan en el ejemplo son homero, bart, lisa, maggie y abe. Siempre que aparezca el átomo homero en mi programa me voy a estar refiriendo a la misma persona. Por eso puedo definir hermano/2 como:

`hermano(H1, H2):- padre(Padre, H1), padre(Padre, H2), H1 \= H2.`

Si los dos individuos que queremos relacionar con el predicado hermano/2 tienen al mismo padre, la relación se va a cumplir (usamos el \\= para asegurarnos que una respuesta posible no sea H1=bart, H2=bart).

Es importante notar que estos individuos no son strings

`?- length("homero",X).`
`X = 6`
`?- length(homero,X).`
`No    <-- por `[ `principio` `de` `universo` `cerrado`](paradigma-logico---introduccion-universo-cerrado.html)

Números
-------

Los números pueden usarse para lo mismo que en cualquier otro paradigma. Los podemos comparar, saber si uno es mayor que otro y usarlos para resolver [ operaciones aritméticas](aritmetica-en-prolog.html). Pero siendo que estamos en lógico y contamos con la idea de inversibilidad y múltiples respuestas también podemos hacer cosas como:

`?- between(3, 7, X).`
`X = 3 ;`
`X = 4 ;`
`X = 5 ;`
`X = 6 ;`
`X = 7 ;`
`No`

Algunas consideraciones que hay que tener con los operadores como el \\=, el &gt;, el + etc.. es que requieren que las variables utilizadas ya se encuentren ligadas. De lo contrario ocurrirá un error en tiempo de ejecución como este:

`?- 3 > A.`
`ERROR: Arguments are not sufficiently instantiated`

De la misma forma, el between que vimos recién sólo es inversible para el tercer argumento, los dos primeros que definen el rango deben ser valores concretos. Esto tiene sentido ya que las posibles respuestas serían infinitas si no se define un máximo y un mínimo.
