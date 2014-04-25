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

Incluso si usamos los imports estáticos de Java, podemos importar métodos estáticos de una clase, para no tener que llamarlos con el nombre de la clase, punto, y el método. Quedaría

`import static org.uqbar-project.CollectionUtils.select;`
`...`
`personas = ...`
`select(personas, new MayorDe18Condicion());`

Ok, igualmente sigue siendo mucho más burocrático y pesado que la implementación en smalltalk.

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

De lo anterior podemos concluir que la Condición en general va a ser un objetito más bien descartable, que lo uso para filtrar una colección en particular y luego lo descarto (más aún si tiene estado, no puedo reutilizar la condición "mayor a &lt;23&gt;" si luego tengo que buscar los mayores a &lt;50&gt;).

Además, de que vamos a tener muchísimas clases implementaciones, según cuántos criterios de filtrado tengamos en el sistema.

Esto es porque en realidad lo que necesitaríamos es expresar un pedacito de código nada más.

Bien, como java no tiene bloques, para apalear un poco este problema, introduce la idea de **clase anónima interna** (del inglés anonymous inner classes). La idea es que en el cuerpo de un método pueda escribir una clase ahí mismo, "inline", es decir sin necesidad de hacerlo en otro archivo. Como voy a implementar con esto un pedacito de código "descartable", no hace falta que le ponga nombre a esta clase.

Entonces, diréctamente instancio la interfaz (o clase abstracta), abro llaves, y ahí mismo la implemento.

Ejemplo:

`public Collection getMayoresDe18() {`
`   Condicion mayoresA18 = new Condicion() {`
`       @Override`
`       public boolean cumple(Object obj) {`
`           return ((Persona) obj).getEdad() > 18;`
`       }`
`   };`
`   return select(personas, mayoresA23);`
`}`

Este ejemplo es equivalente al primero que hicimos. Nótese que estamos haciendo

`variable =  `<nueva clase anónima>

La sintaxis es

` new Interface() {`
`    `
`    // implementación de la interface   `
` `
` }`

Eso me da un objeto, que en este primer ejemplo lo asignamos a una variable local. Podríamos incluso no usar la variable local y diréctamente crearla para pasarla por parámetro al "select".

`public Collection getMayoresDe18() {`
`   return select(personas, new Condicion() {`
`       @Override`
`       public boolean cumple(Object obj) {`
`           return ((Persona) obj).getEdad() > 18;`
`       }`
`     }`
`   );`
`}`

Fíjense que es lo mismo, simplemente metimos todo el código desde el "new Cond... hasta el cierre de llaves de la calse, en el lugar del parámetro.

Así, si lo miran con cariño (mucho cariño), se parece un poco a la implementación con bloques.

Ojo, las reglas son las mismas que cuando definimos una clase en otro archivo. Como que:

-   nos fuerza a implementar todos los métodos abstractos.
-   podemos sobrescribir un método (en caso de hacer una subclase anónima de una clase abstracta).
-   podemos llamar a super (pasa clase abstracta también).
-   etc.

Sin embargo, a diferencia de una clase normal tiene algunas particularidades.

Acceso a la instancia contenedora (Clase.this)
----------------------------------------------

La clase anónima además de poder acceder a su estado interno y mandarle mensajes a los parámetros que recibe en métodos, "ve" y puede también acceder al estado interno y mandarle mensajes a la instancia de la clase que la creó (es decir donde está el código definido).

Rehagamos el ejemplo de la empresa

`  public class Empresa {`
`      public Collection getEmpleados() {`
`          return select(personas, new Condicion() {`
`              @Override`
`              public boolean cumple(Object obj) {`
`                   return ((Persona) obj).getEmpleador() == Empresa.this;`
`              }`
`          });`
`      }`
`  }`

Ven que acá estamos necesitamos comparar el empleador de la persona con la nosotros mismos. El problema es que estamos teniendo código dentro de una clase (anónima de Condicion) que está dentro de otra clase (Empresa). Entonces, no existe un único "this". Tenemos dos. Como regla para toda clase anónima

-   **this**: se refiere al objeto de la clase anónima, en nuestro caso sería la instancia de Condicion.
-   **ClaseContenedora.this**: se refiere al objeto instancia de la clase contenedora de esta anónima. En nuestro caso la Empresa.

Así podríamos mandarle mensajes también

`...`
`            return Empresa.this.esExEmpleado((Persona) obj);`
`...`

Donde "esExEmpleado(Persona p)" sería un método de la Empresa.

O también accederle a una variable de instancia

`...`
`            return ((Persona p)obj).getEmpleador().getNombre().equals(Empresa.this.nombre);`
`...`

Acceso a variables locales (final)
----------------------------------

Existe una particularidad de la implementación de Java, para recrear el último caso en que usábamos un parámetro "edad" para fitrar.

Si escribieramos eso así:

`public Collection getMayoresA(int edad) {`
`   return select(personas, new Condicion() {`
`        @Override`
`        public boolean cumple(Object obj) {`
`            return ((Persona) obj).getEdad() > edad;`
`        }`
`   });`
`}`

Tendrímos un error de compilación al hacer "&gt; edad", es decir al intentar acceder desde el código de la clase anónima al parámetro. Esto es porque la clase anónima si bien tiene acceso al "scope" de variables del método, solo puede acceder a aquellas variables que sean **final**, es decir que sean constantes, que no puedan cambiar. Esto es por una limitación de java. La solución sería agregar el modificador **final** al parámetro

`public Collection getMayoresA(final int edad) {`
`...`

Lo mismo sucede con **referencias locales**

`public Collection getMayoresAlPromedioDeEdad() {`
`   final int promedioDeEdad = this.calcularPromedioDeEdad();`
`   return select(personas, new Condicion() {`
`        @Override`
`        public boolean cumple(Object obj) {`
`            return ((Persona) obj).getEdad() > promedioDeEdad;`
`        }`
`   });`
`} `

Nótese que la referencia local "promedioDeEdad" está declarada como final.

Y si queremos cambiar el valor de la variable local (no puedo hacerla final) ??
-------------------------------------------------------------------------------

Bien, se puede resolver, sin embargo introduce burocracia. Para no cambiar el dominio vamos a tener que caer en un ejemplo de manejo de estructura.

Supongamos que queremos obtener a un diccionario o Mapa con los empleados asociados a su edad expresada en decadas. Ej:

`20 >  Juan(23), Jose (27), Maria (21)`
`30 >  Yesi (30), Nico (36)`
`40 >  Carlos (48)`
`...`

Haríamos una iteración entre 20 y 70, por ejemplo, filtrando los empleados que tengan edad &gt;= i y edad &lt;= i + 9

`public Map`<Integer, Collection>` getEmpleadosPorEdad() {`
`    Map`<Integer, Collection>` empleadosPorEdades = new HashMap`<Integer,Collection>`();`
`    for (int i = 20; i <= 70; i += 10) {`
`         Collection empleadosEncontrados = select(this.getEmpleados(), new Condicion() {`
`              @Override`
`              public boolean cumple(Object obj) {`
`                  int edad = ((Persona)obj).getEdad();`
`                  return edad >= i && edad <= (i + 9)`
`              }`
`         });`
`         empleadosPorEdades.put(i, empleadosEncontrados);`
`    }`
`}`

Bien, esto no compilaría, porque desde la clase anónima estamos usando la variable local "i", que no es final. Si procedemos a ponerle "final" en la declaración, ahora no va a compilar el "for" en la parte del "i+=10" porque no se puede modificar una variable final. Entonces, estamos en una encrucijada. La forma de resolverla, es mover la clase anónima a otro método, donde la "i" sí la podemos declarar como final. Para esto podemos extraer el filtrado a otro método, y desde este pasarle la i como parámetro.

`public Map`<Integer, Collection>` getEmpleadosPorEdad() {`
`    Map`<Integer, Collection>` empleadosPorEdades = new HashMap`<Integer,Collection>`();`
`    for (int i = 20; i <= 70; i += 10) {`
`         Collection empleadosEncontrados = this.getEmpleadosEnLaDecadaDel(i);`
`         empleadosPorEdades.put(i, empleadosEncontrados);`
`    }`
`}`

Y el nuevo método:

`public Collection getEmpleadosEnLaDecadaDel(final int i) {`
`    return select(this.getEmpleados(), new Condicion() {`
`          @Override`
`          public boolean cumple(Object obj) {`
`              int edad = ((Persona)obj).getEdad();`
`              return edad >= i && edad <= (i + 9)`
`          }`
`    });`
`}`

Como ven el parámetro "i" nunca cambia en este método, así que sí podemos declararlo como final. Mientras que en el for puede seguir siendo "no final".
