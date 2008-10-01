Ojo con los espacios
--------------------

Hay un lugar en donde no puede haber ningún espacio, que es entre el nombre del predicado o functor y el paréntesis que abre. O sea, ninguna de las siguientes cláusulas compila

`   bueno(X):- not (malo(X)).  `
`   esTierno(P):- forall (leGusta(P,A), dulce(A)).`
`   leGusta (copion,C):- leGusta(pepe,C).`
`   leGusta(copion,C):- leGusta (pepe,C).`
`   tiene(pepe,libro (octaedro,cortazar)).`

hay que poner

`   bueno(X):- not(malo(X)).  `
`   esTierno(P):- forall(leGusta(P,A), dulce(A)).`
`   leGusta(copion,C):- leGusta(pepe,C).`
`   tiene(pepe,libro(octaedro,cortazar)).`

los espacios en otros lados no molestan, p.ej.

`   leGusta( copion , C ) :- leGusta( pepe , C ) .`
`   tiene( pepe , libro( octaedro , cortazar ) ) .`
