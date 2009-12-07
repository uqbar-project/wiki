Conviene hacer las comparaciones con `=` y no con `==`.

La razón contada muy rápido: imaginate que por alguna razón en la imagen de Smalltalk (o de cualquier lenguaje que trabaja con objetos) en un momento hay dos String "hola", que están apuntados por las variables str1 y str2. La comparación

`   str1 = str2`

da true, porque representan al mismo ente. Pero la comparación

`   str1 == str2`

da false, porque no son exactamente el mismo objeto, la imagen los reconoce como distintos.
