Lo que tiene que estar seguro para aprobar el parcial de funcional es [Orden Superior](orden-superior.html), [Aplicación Parcial](aplicacion-parcial.html) y [Composición](composicion.html).

Algunas consideraciones que te llevan para el lado de la aprobación:

-Entender que cuando se aplica una función parcialmente lo que se obtiene es otra función con n - m parámetros (siendo n los parámetros de tu función original y m los que le aplicaste).

-Entender bien la diferencia entre aplicar funciones y componer funciones entre sí (nunca intentar componer cosas que no sean funciones o que al no estar suficientemente aplicadas esperen más de un parámetro)

-Usar buenas [abstracciones](abstraccion.html) tanto propias (defininiendo funciones auxiliares que ayuden a dividir el problema el problemas más chicos) como existentes, por ejemplo en vez de hacer una función recursiva para trabajar con una lista usar funciones como filter, map, all, any o algún fold que se adecúe al problema

-Pensar en los tipos de las cosas que reciben y retornan las funciones para asegurarte de que lo que estás haciendo tiene sentido. Si te cuesta el proceso de inferir el tipo de una función acá hay un ejemplito que está bueno para entender cómo se hace: [Cálculo del tipo de una función en Haskell\#Ejemplo un poco mas heavy](calculo-del-tipo-de-una-funcion-en-haskell-ejemplo-un-poco-mas-heavy.html)

Es importante no cometer errores de los que aparecen acá: [Errores comunes al comenzar a trabajar con Haskell](errores-comunes-al-comenzar-a-trabajar-con-haskell.html). Estos errores vienen de la mano de no tener claros los conceptos de orden superior, aplicación parcial y composición, con lo cual se consideran invalidantes.
