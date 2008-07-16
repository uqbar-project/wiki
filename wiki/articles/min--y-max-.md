ejemplo: un remis cobra 2 pesos por kilómetro, mínimo 5 pesos. Tengo que escribir el método precioViaje: en la clase Remis. ¿Hago esto?

`   precioViaje: unViaje`
`       | precio |`
`       precio := 2 * unViaje distancia.`
`       precio >= 5 ifTrue:[^precio] ifFalse:[^5]`

nuuuuuuu ... los programadores objetosos con estilo sólo usan ... min: y max:. Estos mensajes los entienden los números, reciben otro número como parámetro, y devuelven el más chico / más grande entre self y el parámetro. Pueden probar esto en un workspace

`   3 min: 8`
`   3 max: 8`
`   3 min: 1`
`   3 max: 1`

Entonces ¿cómo queda el precioViaje:? Lo que tengo que devolver es el máximo entre 5 y el resultado de la cuenta (piénsenlo ...), entonces queda así

`   precioViaje: unViaje`
`       ^5 max: (2 * unViaje distancia)`

bel-leza

Para practicar, hagamos que a un remis le pueda preguntar

-   la cantidad de pasajeros legal, que es la que tiene, pero nunca puede ser más de 5. O sea, si en realidad lleva 7 personas, la cantidad legal es 5.
-   el consumo por combustible, que es 1 litro cada 8 km, mínimo 2 litros.

Los remises entienden distancia y cantPasajeros
