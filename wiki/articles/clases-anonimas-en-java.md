Motivación: ausencia de bloques
-------------------------------

La motivación más importante para entender las clases anónimas es la ausencia [ bloques (o closures)](bloques.html) en el lenguaje Java. Java 8 recién es la primer versión que introduce un mecanismo de bloques (ver [Lambdas\_en\_Java\_8](lambdas-en-java-8.html))

Veamos un ejemplo:

En smalltalk para seleccionar los elementos de una colección que cumplen cierta condición enviamos el mensaje "select:" Por ejemplo, para obtener las personas mayores de 18 años haríamos:

`personas select: [:p | p edad > 18 ]`

Ejemplo en Java Sin clases anónimas
-----------------------------------

Cómo haríamos eso en java ?

Primero que nada no existe un método "select" ni nada parecido en las colecciones (interfaz Collection). Así que tenemos que hacer el método nosotros.

Hacemos un método estático como una utilidad, al que le tenemos que pasar la colección como parámetro.

`public class CollectionUtils {`
`   public static Collection`<T>` select(Collection`<T>` coleccion, <<<< condicion >>>>) {`
`       ...`
`   }`
`}`

Obviamente también tenemos que pasarle de alguna forma la condición !

Ahora bien, en Java no existen los bloques o closures, con lo cual, la única opción que tenemos es pasar un objeto. Así que tenemos que modelar el concepto de **Condicion**.

`public interface Condicion {`
`  public boolean cumple(Object obj);`
`}`

Como la condición es lo que va a tener que implementar el "usuario" de este método, es decir quien quiera filtrar una colección, la definimos como una Interfaz de java, para no poner una restricción sobre una superclase (la otra opción sería que fuera una clase abstracta).

Fíjense que es un objeto que tiene una \[<http://en.wikipedia.org/wiki/Single_responsibility_principle>| \[única responsabilidad\]\] bien identificada. Dado un objeto que le paso por parámetro me sabe decir si cumple o no con la condición.

Entonces hacemos una primer implementación de nuestro ejemplo de filtrar mayores de 18 creando una clase normal que implemente esa interfaz.

`class MayorDe18Condicion implements Condicion {`
`   @Override`
`   public boolean cumple(Object obj) {`
`       return ((Persona) obj).getEdad() > 18;`
`   }`
`}`

Y para filtrar una colección

`personas = ...`
`CollectionUtils.select(personas, new MayorDe18Condicion());`

Incluso si usamos los imports estáticos de Java, podemos importar métodos estáticos de una clase, para no tener que llamarlos con el nombre de la clase, punto, y el método. Quedaría:

`import static org.uqbar-project.CollectionUtils.select;`
`...`

`personas = ...`
`select(personas, new MayorDe18Condicion());`

Ok, igualmente sigue siendo mucho más burocrático y pesado que la implementación en smalltalk. Además, los métodos estáticos en Java no pueden ser polimórficos. Son cosas raras y no métodos normales que puedan ser sobrescrito o implementado en diferentes formas.

Igualmente, lo que más nos molesta es que tenemos que crear una nueva clase por cada condición por la que querramos filtrar. Es bastante molesto eso. Ejemplo, queremos filtrar las personas casadas..

`class EsCasadaCondicion implements Condicion {`
`   @Override`
`   public boolean cumple(Object obj) {`
`       return ((Persona) obj).esCasada();`
`   }`
`}`

Y luego

` casados = select(personas, new EsCasadaCondicion());`

Accediendo a referencias de otro Objeto (sin anónimas)
------------------------------------------------------

Qué pasa si ahora queremos desde la condición utilizar una estado interno del objeto que está filtrando. Por ejemplo, desde una Empresa, queremos obtener sus empleados. Para eso, filtramos de la colección de personas a aquellas que trabajen para esta empresa.

`public class Empresa {`
`   `
`   public Collection getEmpleados() {`
`       return select(personas, new TrabajaEnCondicion(this));`
`   }     `
`   `
`}`

Como se ve necesitamos pasarle la empresa a la condición. Es una condición que tiene estado. En este caso se pasa a sí mismo todo el objeto Empresa.

`class TrabajaEnCondicion implements Condicion {`
`   private Empresa empresa;`

`   public TrabajaEnCondicion(Empresa empresa) {`
`       this.empresa = empresa;`
`   }`

`   @Override`
`   public boolean cumple(Object obj) {`
`       return ((Persona) obj).getEmpleador() == this.empresa;`
`   }`
`}`

Cada vez más código ! :S Necesitamos declarar el atributo (estado interno) y recibirlo en el constructor.

Bastante burocrático nuévamente. En smalltalk sería

` Empresa>>empleados`
`     ^personas select: [:p | (p empleador) = self]`

Accediendo a referencias locales (sin anónimas)
-----------------------------------------------

Introducimos otra variante, otra situación normal.

Queremos poder preguntarle a la empresa cuales son todos los empleados con más de "x" años. Es decir, no tenemos un valor único y fijo como antes, de 18, sino que lo recibimos como parámetro.

` public Collection getEmpleadosMayoresA(int anios) {`
`      return select(this.getEmpleados(), new MayoresA(anios));`
` }`

De nuevo, se ve acá que tenemos que pasarle todo el estado que necesite la condición, como parámetro en la construcción. En este caso es un parámetro. Podría ser incluso una variable local.

Aparece la Clase Anónima
------------------------

impl para el ejemplo. Reflexionar un poco.

Particularidades de la Implementación de Clases Anónimas
--------------------------------------------------------

1- scope solo de var finals 2- no puede tener constructores (?)
