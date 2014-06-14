Un objeto es inmutable si no puede cambiar su estado interno (su conjunto de variables) después de su creación.

Los Strings son un ejemplo de objetos inmutables. Cualquier operación que hagan sobre un string (concatenación, cambiar a mayúscula o minúscula, etc) sólo retorna otro string, el receptor nunca se modifica.

Usando [ métodos de clase](variables-y-metodos-de-clase.html) es posible parametrizar la construcción del objeto para que sea inicializado en la misma operación. Si además el objeto no exhibe el comportamiento para settear sus atributos, sus usuarios no podrán alterar su estado interno luego de ser creado.
