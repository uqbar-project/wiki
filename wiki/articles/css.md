Es la forma más recomendada de modificar las cuestiones estéticas de una página [HTML](html.html)

Un CSS permite definir estilos que se asocian a las diferentes partes de la página, de tres maneras distintas:

-   Por tag, es decir a todos los tags de un tipo (por ejemplo h1, h2, etc).
-   Por clase, es decir a los tags a los que se les haya indicado un estilo determinado (mediante el atributo **class**).
-   Por id, es decir a un elemento específico de la página según su **id**.

Las reglas se aplican en cascada, esto significa dos cosas:

1.  En primer lugar cada componente hereda determinados estilos de sus contenedores, por ejemplo un td hereda los del tr y del table correspondientes. Los estilos que apliquen al componente específico sobreescriben a los del contenedor, pero aquellos que no estén especificados se heredan. No todas las indicaciones de estilo son "heredables" (inheritable en inglés), es importante entender el comportamiento de cada una de las diferentes indicaciones de estilo.
2.  En segundo lugar sobre cada componente pueden aplicarse más de un estilo, que matcheen con ese componente según su tag, class y id respectivamente. Esos diferentes estilos se van a combinar permitiendo que el estilo más específico sobreescriba los estilos más generales, pero aún manteniendo las indicaciones correspondientes al estilo más general que no sean redefinidas.

