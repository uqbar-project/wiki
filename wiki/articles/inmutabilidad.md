El concepto de inmutabilidad está asociado a la ausencia de cambio. En los paradigmas Funcional y Lógico, la inmutabilidad está garantizada, ya que no es posible modificar los datos con los que trabaja una función o un predicado, en todo caso lo que se puede hacer para emular un cambio sobre una estructura es retornar una nueva estructura a partir de la original con la información que tendría como consecuencia de la transformación deseada.

Decimos que un objeto es inmutable si no puede cambiar su estado interno (su conjunto de atributos) después de su inicialización. Si la interfaz del objeto tiene una forma de inicializar sus variables, pero no exhibe el comportamiento para settear sus atributos, sus usuarios no podrán alterar su estado interno más adelante. Para construir un objeto ya inicializado solemos usar [ métodos de clase](variables-y-metodos-de-clase.html), sólo habrá que mandarle al objeto un mensaje distinto (por ejemplo, si fuera un alumno que conoce su nombre y su promedio, inicializarConNombre:yPromedio:)

Los Strings son un ejemplo de objetos inmutables, cualquier operación que hagan sobre un string (concatenación, cambiar a mayúscula o minúscula, etc) sólo retorna otro string, el receptor nunca se modifica.

Que un objeto sea inmutable permite que pueda ser compartido por varios objetos sin que puedan afectarse entre sí, ya que no hay nada que puedan hacer sobre ese objeto compartido que produzca un cambio visible para el otro objeto que lo conoce.

Es importante tener en cuenta que no alcanza en algunos casos con que el objeto no exhiba comportamiento que permita mutarlo, ya que podría tener un atributo que referencie a un objeto que sí es mutable. Luego si alguien le manda un mensaje que retorna a ese objeto mutable, y a ese objeto le manda un mensaje que lo modifica, el objeto inicial habrá sufrido cambios.

Volviendo al caso de los Strings, estos objetos están compuestos por otros objetos que representan a los caracteres, qué pasa si al string 'hola' le pido su primer elemento y a ese caracter $h le mando un mensaje para que se cambie a mayúscula, luego el string original ya no sería 'hola' sino, 'Hola', no? Eso no es un cambio sobre el string también??

Bueno, como resulta que los caracteres también son inmutables, realmente no hay nada que se pueda hacer para que el string cambie.

Entonces, si estamos haciendo nuestros propios objetos inmutables, hay que tener en cuenta si los objetos que conocen pueden o no cambiar su estado. En el caso de que no, listo, pero si sí pueden hay que plantearse si dichos objetos no deberían ser también inmutables, y en el caso de no querer que así sea, retornar copias de los mismos cuando sea necesario. De esa forma, quien envíe el mensaje que retorna uno de estos objetos mutables, pueda realizar modificaciones sobre el mismo sin que el objeto inmutable se vea afectado.

Otra cosa a mencionar es sobre los objetos inmutables es que la [igualdad ya no se basa en la identidad](igual-o-identico-----vs---.html). Debería ser cierto por ejemplo que 'hola' sea igual a 'ho' concatenado con 'la', independientemente de que sean o no el mismo objeto. La igualdad termina dependiendo de los valores de sus atributos (o un subconjunto de ellos).
