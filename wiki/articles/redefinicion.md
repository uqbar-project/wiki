Para revisar.

### Por Germán

Todo lo de abajo es bastante informal, porque es un concepto bastante simple como para tener una definición super precisa

Redefinir: Dícese de la acción de crear un nuevo método con el mismo nombre de uno heredado (o sea, heredado por algún "ancestro")

En un ambiente de objetos basado en clases los ancestros son la superclase, la superclase de la superclase y así; el lugar donde creamos el nuevo método es una clase.

La redifinición se puede usar para hacer lo mismo que antes pero con alguna variante (acá mayormente se usa super) o para hacer algo que nada que ver (es una redefinición sin super).

Está bastante relacionado con el concepto de Method Lookup (proceso por el cual se engancha el mensaje enviado con el método a ejecutar)

Redefinición != Sobrecarga (aunque este concepto no lo mencionamos mucho en la materia)

### Por Ricardo Scattini

La redefinición se produce cuando una clase vuelve a definir, o sea redefine, alguno de los métodos heredados de su superclase. El nuevo método sustituye al heredado para todos los objetos de la clase que lo ha redefinido, de manera que sus objetos tienen un comportamiento modificado respecto de los objetos de la superclase.

Así, la redefinición permite que al definir una nueva clase sus objetos no sólo extiendan o amplíen el funcionamiento de los objetos de la superclase, sino también los modifiquen, ajustándolo a los requerimientos y necesidades específicas para los cuales se creó la subclase.
