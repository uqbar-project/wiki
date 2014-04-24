Motivación: ausencia de bloques
-------------------------------

La motivación más importante para entender las clases anónimas es la ausencia [ bloques (o closures)](bloques.html) en el lenguaje Java. Java 8 recién es la primer versión que introduce un mecanismo de bloques (ver [Lambdas\_en\_Java\_8](lambdas-en-java-8.html))

Veamos un ejemplo:

En smalltalk para seleccionar los elementos de una colección que cumplen cierta condición enviamos el mensaje "select:" Por ejemplo, para obtener las personas mayores de 18 años haríamos:

`personas select: [:p | p edad > 18 ]`

Cómo haríamos eso en java ?

Primero que nada no existe un método "select" ni nada parecido en las colecciones (interfaz Collection). Así que tenemos que hacer el método nosotros.

Hacemos un método estático como una utilidad, al que le tenemos que pasar la colección como parámetro.

`public class CollectionUtils {`
`   public static Collection`<T>` select(Collection`<T>` coleccion, <<<< condicion >>>>) {`
`       ...`
`   }`
`}`

Ejemplo con colecciones,

-   interfaz predicate,
-   con una clase normal.
-   scope al objeto original (atributo)
-   scope a variables locales (más atributos)

Aparece la Clase Anónima
------------------------

impl para el ejemplo. Reflexionar un poco.

Particularidades de la Implementación de Clases Anónimas
--------------------------------------------------------

1- scope solo de var finals 2- no puede tener constructores (?)
