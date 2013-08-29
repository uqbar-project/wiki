La redefinición se produce cuando una clase vuelve a definir, o sea redefine, alguno de los métodos heredados de su superclase. El nuevo método sustituye al heredado para todos los objetos de la clase que lo ha redefinido, de manera que sus objetos tienen un comportamiento modificado respecto de los objetos de la superclase.

Así, la redefinición permite que al definir una nueva clase sus objetos no sólo extiendan o amplíen el funcionamiento de los objetos de la superclase, sino también los modifiquen, ajustándolo a los requerimientos y necesidades específicas para los cuales se creó la subclase.

La redifinición se puede usar para definir la misma operación que antes pero con alguna variante (acá mayormente se usa [super](super.html)) o para definir la misma operación sin código en común con el heredado (es una redefinición sin super, se puede ver un ejemplo en [Prototipado vs Clases\#Redefinir comportamiento](prototipado-vs-clases-redefinir-comportamiento.html)).
