En el paradigma orientado a objetos, cuando se suscita el problema de que existan **diferentes objetos con igual comportamiento pero diferente identidad y estado interno**, podemos abordarlo de diferentes formas. Dos esquemas muy usados son el de **[clases](clases.html)** y el que se basa en **[prototipos](prototipado.html)**. Cada lenguaje (o herramienta) suele implementar una sóla de éstas opciones, aunque existen variaciones. Los lenguajes más tradicionales (como Java, Smalltalk, y C\#) usan el esquema de clases y [herencia](herencia.html), y en otros lugares (como Javascript, ó en el Object Browser para Smalltalk), se usa el esquema de prototipado.

Más allá de cuál de estos esquemas usemos, la base es la misma: objetos que se mandan mensajes en un ambiente aprovechando las ideas de encapsulamiento, delegación y polimorfismo.

La idea de este artículo es ver cómo se puede trabajar en cada esquema para solucionar los mismos problemas.

Compartir comportamiento entre varios objetos
---------------------------------------------

¿Qué pasa si pepita no es la única golondrina que nos interesa tener en nuestro programa?. También están josefa y pepona que comen y vuelan como pepita y también deben tener una energía que podamos monitorear. Estaría bueno que josefa y pepona puedan tener el mismo comportamiento que pepita sin tener que definir lo mismo 3 veces. Aquí es donde entran los conceptos de clase y de prototipo, dependiendo de las herramientas que tengamos disponibles.

Cuando existe la idea de clase en nuestra implementación del paradigma, el comportamiento se define una sola vez en la clase y tanto pepita como josefa y pepona pasan a ser **instancias** de esta clase. No podemos sólo tener a pepita con su propio código porque todos los objetos son instancias de una clase que las define. Cuando cualquiera de las golondrinas reciba el mensaje vola:, lo van a entender y buscarán el método que lo define en la clase Golondrina.

Si no tenemos la idea de clase, el mecanismo que necesitamos para que josefa y pepona compartan el código con pepita es la **colonación**. Clonando a pepita podemos crear un nuevo objeto (josefa) que entiende los mismos mensajes y tiene sus mismas variables, y además conoce a pepita como su **prototipo**.

Si queremos cambiar el comportamiento general de las golondrinas alcanza con realizar estos cambios sobre la clase Golondrina o sobre el prototipo pepita. Al decir que un objeto es clon de otro estamos estableciendo una relación tan fuerte entre ellos como al decir que un objeto es instancia de una clase al trabajar en el otro esquema.

Compartir comportamiento y agregar más en algunos objetos
---------------------------------------------------------

Queremos que pepona que es una golondrina perezosa, entienda el mensaje descansar que hace subir su energía en 50 joules. Sin embargo pepita y josefa que son golondrinas normales deberían seguir teniendo el mismo comportamiento (no entenderían descansar).

En el esquema de prototipado podemos simplemente modificar a pepona como lo haríamos con cualquier otro objeto, sin importar si fue clonado a partir de pepita o no.

`#pepona `
`>> descansar`
`  energia := energia + 50`

Cuando mandemos el mensaje `pepona` `descansar` lo va a entender (ya que tiene un método propio para este mensaje), va a ejecutar su método descansar y su energía se incrementará. Si mandáramos `pepita` `descansar` tendríamos un error porque pepita no entiende ese mensaje.

¿Qué pasaría si estuviéramos trabajando con el esquema de clases? Siempre que aparece comportamiento nuevo, necesitamos crear otra clase que lo incluya, con lo cual necesitaríamos crear una clase GolondrinaPerezosa que [herede](herencia.html) de Golondrina y definir allí el método \#descansar. De esta forma, pepona ya no debería ser una instancia de Golondrina sino de GolondrinaPerezosa.

`pepona := GolondrinaPerezosa new.`
`pepona descansar. "esto va a funcionar"`
`pepita := Golondrina new.`
`pepita descansar. "pepita no va a entender el mensaje descansar"`

Si lo que queremos es agregar algún atributo nuevo para pepona, estaríamos en la misma situación, habría que agregarlo en el objeto \#pepona o en la clase GolondrinaPerezosa respectivamente.

Redefinir comportamiento
------------------------

Siguiendo con pepona, la golondrina perezosa, sabemos que cuando vuela gasta más energía para despegar (15 en vez de 10). Una primer solución podría ser [redefinir](redefinicion.html) el método vola:

`#pepona / #GolondrinaPerezosa`
`>> vola: unosKilometros`
`  energia := energia - (unosKilometros * 5 + 15)`

Cuando le mandemos el mensaje `pepona` `vola:` `10`, buscará una definición propia (o en su clase) para vola: y al encontrarla ejecutará ese método. Haber agregado este método no cambia el comportamiento de pepita y josefa, que es lo que queríamos.

Sin embargo podríamos dar un paso más para evitar la repetición de código, siendo que lo único que cambia es el valor de la energía para despegar. Una solución mejor sería:

`#pepita / #Golondrina`
`>> vola: unosKilometros`
`  energia := energia - (unosKilometros * 5 + self energiaParaDespegar)`
`>> energiaParaDespegar`
`  ^ 10`
`#pepona / #GolondrinaPerezosa`
`>> energiaParaDespegar`
`  ^ 15`

Ahora, cuando le mandemos el mensaje `pepona` `vola:` `10`, buscará una definición propia (o en su clase) para vola:, no la va a encontrar, con lo cual seguirá buscando en su prototipo (o la superclase de su clase) donde sí existe. Al ejecutar esa definición, se enviará a sí misma el mensaje \#energiaParaDespegar; empieza a buscar un método con ese nombre en sí misma (o en su clase GolondrinaPerezosa), lo encuentra y retorna 15, valor que se usará para completar la ejecución de vola:

Si le mandamos el mensaje `pepita` `vola:` `10`, encontrará y ejecutará la definición en sí misma (o en su clase Golondrina) al igual que para el mensaje \#energiaParaDespegar que retornará el valor 10.
