---
layout: article
title: Data  definiendo nuestros tipos en haskell
---

Cómo están definidos los Tipos en Haskell?
------------------------------------------

Se utiliza data para definir un tipo de dato, por ejemplo si quisiéramos definir el tipo Bool escribiríamos

`data Bool = <...> `

Qué vamos a escribir en &lt;...&gt; los posibles valores que tiene ese tipo separados por un pipe (|)

`data Bool = False | True`

Con esta notación podríamos pensar que el tipo Int está escrito de la siguiente manera

`data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647  `

**Nota: Int no está definido de esta manera pero sirve para entender el uso de data**

Si quisiéramos escribir el tipo ColorPrimario cuales serían sus posibles valores?

`data ColorPrimario = Rojo | Amarillo | Azul`

Tuplas tuplas tuplas
--------------------

Hasta ahora cuando teníamos que representar un valor, compuesto por otros valores de tipos distintos, usábamos Tuplas. Vamos a hacer un ejemplo con tuplas de 2 elementos por simplicidad, pero lo mismo se aplica para tuplas de n elementos

Si queremos representar un alumno por su nombre (un String) y sus notas (una lista de Int = \[Int\])

`cursoK9 = [("Federico",[2,3]),("Líder",[10,10,10,10,10]),("Germain",[8,9,10])]`
`empezoMal unAlumno = 4 > head (notasAlumno unoAlumno)`

Si queremos representar una película por su título (unString) y los puntajes que le ponen los críticos en imdb (una lista de Int = \[Int\])

`pelis = [("Pedornia", [0,0,-3,-666]),("Pulp Fiction", [9,10,9]),("Fight Club", [3,8,8,9,9,10])]`

también tengo que definir funciones para interactuar con alumnos y películas

`nombreAlumno unAlumno = fst unAlumno`
`notasAlumno unAlumno = snd unAlumno`
`tituloPelicula unaPelicula = fst unaPelicula`
`puntajesPelicula unaPelicula = snd unaPelicula`

Si usamos lo que definimos arriba como un solo programa (un solo .hs), podemos ver que:

-   La función nombreAlumno es igual a la función tituloPelicula
-   La función notasAlumno es igual a la función puntajesPelicula

Nada me impide consultar:

`> puntajesPelicula ("Líder",[10,10,10,10,10])`
`[10,10,10,10,10]`

`> empezoMal (head pelis)`
`True`

Todo esto es posible porque si miramos los [tipos que infiere Haskell](inferencia-de-tipos.html) no existe diferencia entre una película y un alumno, para Haskell los alumnos y películas son sólo tuplas de 2 elementos.

Ejemplo:

`> puntajesPelicula ([1,2,3],(True,"hola"))`
`(True,"hola")`

Ya que Haskell es un lenguaje "que se fija mucho en los tipos", nos gustaría que un caso como los de arriba nos tirará error (donde en vez de mandar un alumno o una película según corresponda, enviamos cualquier otra cosa).

Definiendo nuevos tipos
-----------------------

Para poder diferenciar a un alumno de una película y a ambos de una tupla, tenemos que definir un nuevo tipo. Eso se hace usando data:

`data NuevoTipo = Constructor Tipo1 Tipo2 ... Tipon`

En nuestro ejemplo:

`data TipoAlumno = Alumno String [Int]`
`data TipoPelicula = Pelicula String [Int]`

Ahora, para obtener un nuevo alumno o una nueva película, tenemos que usar el "Constructor"

`cursoK9 = [Alumno "Federico" [2,3], Alumno "Líder" [10,10,10,10,10], Alumno "Germain" [8,9,10]]`

`-- No cambia`
`empezoMal unAlumno = 4 > head (notasAlumno unoAlumno)`

`pelis = [Pelicula "Pedornia" [0,0,-3,-666], Pelicula "Pulp Fiction" [9,10,9], Pelicula "Fight Club" [8,8,8,9,9,10]]`

`-- Ahora estas funciones usan Pattern-Matching!`
`nombreAlumno (Alumno nombre notas) = nombre `
`notasAlumno (Alumno nombre notas) = notas`
`tituloPelicula (Pelicula nombre notas) = nombre`
`puntajesPelicula (Pelicula nombre notas) = notas`

Es importante remarcar que al hacer esto un alumno o una película **YA NO ES UNA TUPLA**

`fst :: (a,b) -> a`
`nombreAlumno :: TipoAlumno -> String`
`tituloPelicula :: TipoPelicula -> String`
`snd :: (a,b) -> b`
`notasAlumno :: TipoAlumno -> [Int]`
`puntajesPelicula :: TipoPelicula -> [Int]`

`cursoK9 :: [ TipoAlumno ]`
`empezoMal :: TipoAlumno -> Bool`

`pelis :: [ TipoPelicula ]`

Ejemplos:

`fede = Alumno "Federico" [2,3]`
`ger = Alumno "Germain" [8,9,10]`
`pulp = Pelicula "Pulp Fiction" [9,10,9]`

`> fst `
`Error (fst espera una tupla y fede es de TipoAlumno)`
`> nombreAlumno fede`
`"Federico"`
`> nombreAlumno pulp`
`Error (nombreAlumno espera un TipoAlumno y pulp es de TipoPelicula)`
`> puntajesPelicula fede`
`Error (puntajesPelicula espera un TipoPelicula y fede es de TipoAlumno)`
`> puntajesPelicula pulp`
`[9,10,9]`
`> empezoMal fede`
`True`
`> empezoMal (head pelis)`
`Error (empezoMal espera TipoAlumno y el primer elemento de pelis es TipoPelicula)`

Deriving
--------

Es muy común querer comparar por igualdad y mostrar por pantalla un valor que tiene un tipo definido por nosotros.

`> head cursoK9`
`Error (el TipoAlumno no tiene la restricción `[`Show`](tipos-de-haskell-la-restriccion-show.html)`)`

Para que esto funcione deberíamos:

-   Decir que TipoAlumno es un tipo que pertenece a la restricción Show
-   Definir la función show para un TipoAlumno

En vez de hacer esto a mano, y debido a que los elementos que forman un Alumno SI tienen la restricción Show, podemos hacer que el TipoAlumno "derive" esa restricción

`--Lo único que hay que agregar es deriving (Show)`
`data TipoAlumno = Alumno String [Int] deriving (Show)`

Con este agregado podemos hacer

`> head cursoK9`
`Alumno "Federico" [2,3]`

Ahora sí hacemos lo siguiente

`> fede == ger`
`Error (el TipoAlumno no tiene la restricción `[`Eq`](tipos-de-haskell-typeclasses.html)`)`

También parece común querer preguntar si dos alumnos son iguales (o distintos), pasa lo mismo que con Show, nos gustaría que el TipoAlumno pertenezca a la restricción Eq

`--Lo único que hay que agregar es deriving (Show,Eq)`
`data TipoAlumno = Alumno String [Int] deriving (Show,Eq)`

Con este agregado podemos hacer

`> fede == ger`
`False`
`> Alumno "Roberto" [7,8,9] == Alumno "Huberto" [7,8,9]`
`False`
`> Alumno "Roberto" [7,8,9] == Alumno "Roberto" [7,8,9]`
`True`

También se puede utilizar el deriving con la clase Ord

`data Nota = Insuficiente | Regular | Bien | MuyBien`

Al hacer:

`Main> Insuficiente > Regular`
`ERROR: No instance for (Ord Nota)`

Esto se debe a que el tipo Nota no cumple con la restricción Ord, por defecto se considera a los valores en forma ascendente de izquierda a derecha (i.e. Insuficiente &lt; Regular &lt; Bien &lt; MuyBien).

Para obtener este comportamiento en los valores del tipo Nota lo único que debemos hacer es "derivar" la restricción Ord

`data Nota = Insuficiente | Regular | Bien | MuyBien deriving Ord`

`Main> Insuficiente > Regular`
`False`

Agrandando nuestro sistema
--------------------------

Es muy común hacer funciones para obtener los valores que forman nuestro individuo compuesto.

Imaginen que ahora queremos agregarle a nuestro TipoPelicula (además del nombre y sus puntajes), el nombre del director, el nombre de los actores principales y el año en que se estrenó.

`data TipoPelicula = Pelicula String String [String] Int [Int]`

Lo primero que notamos es que no es tan fácil identificar cada elemento. Para eso existe la posibilidad de declarar sinónimos de tipo usando type. Un sinónimo muy útil que ya viene definido en Haskell es:

`-- Se usa type cuando queremos declarar un sinónimo de tipos`
`type String = [Char]`

No existe un tipo String, de hecho cuando preguntamos a Haskell siempre nos responde \[ Char \]

`> :t "hola"`
`"hola" :: [ Char ]`

Pero podemos usar String en nuestras definiciones como sinónimo de \[ Char \]

En el ejemplo de las películas podemos hacer algo como

`type Titulo = String`
`type NombreDirector = String`
`type Puntajes = [Int]`

`data TipoPelicula = Pelicula Titulo NombreDirector [String] Int Puntajes deriving (Show,Eq)`
`narnia = Pelicula "Pedornia" "Andrew Adamson" ["Tilda Swinton", "Georgie Henley","William Moseley"] 2005 [0,0,-3,-666]`
`pulp = Pelicula "Pulp Fiction" "Quentin Tarantino" ["John Travolta", "Uma Thurman", "Samuel L. Jackson"] 1994 [9,10,9]`
`fc = Pelicula "Fight Club" "David Fincher" ["Brad Pitt", "Edward Norton", "Helena Bonham Carter"] 1999 [8,8,8,9,9,10]`

Lo cual mejora la expresividad de la definición.

Otro tema es que tenemos que definir nuevamente funciones como tituloPelicula y puntajesPelicula:

`tituloPelicula (Pelicula nombre director actores anioEstreno notas ) = nombre`
`puntajesPelicula (Pelicula nombre director actores anioEstreno notas ) = notas`

Como en cualquier otro programa, las variables que no nos interesan en absoluto pueden ser reemplazadas por la variable anónima

`tituloPelicula (Pelicula nombre _ _ _ _ ) = nombre`
`puntajesPelicula (Pelicula _ _ _ _ notas ) = notas`

`--También tenemos que definir funciones para el resto de los campos`
`directorPelicula (Pelicula _ director _ _ _ ) = director`
`actores (Pelicula _ _ actores _ _ ) = actores`
`anioEstreno (Pelicula _ _ _ anio _ ) = anio`

Una forma más rápida de definir este tipo de funciones es usando **Type Records** (solo disponible en GHC, no en Hugs)

En vez de definir sólo los tipos de los valores que van a estar en la película, también agregamos en la definición el nombre de la función por el cual queremos obtener dicho valor

`-- Al utilizar la notación de registro hay que encerrar la definición de los campos entre llaves { } y separar cada campo por comas ,`
`data TipoPelicula = `
`  Pelicula`
`    {tituloPelicula :: String ,`
`    directorPelicula :: String,`
`    actores :: [String],`
`    anioEstreno :: Int,`
`    puntajesPelicula :: [Int]}`
`    deriving (Show,Eq)`

Con esta definición automaticamente Haskell define por nosotros las funciones tituloPelicula, puntajesPelicula, directorPelicula, actores y anioEstreno. El dominio de cada una de estas funciones es TipoPelicula y retornan lo que corresponda en cada caso.

Además cuando querramos obtener una nueva Película, podemos hacer

`pulp = Pelicula "Pulp Fiction" "Quentin Tarantino" ["John Travolta", "Uma Thurman", "Samuel L. Jackson"] 1994 [9,10,9]`

O bien

-- Usando la notación record es más claro a que campo pertenece cada valor y no es necesario seguir un orden en los valores mientras se indique a que campo pertenece

`pulp = `
`  Pelicula{`
`    tituloPelicula = "Pulp Fiction",`
`    directorPelicula = "Quentin Tarantino",`
`    anioEstreno = 1994,`
`    puntajesPelicula = [9,10,9],`
`    actores = ["John Travolta", "Uma Thurman", "Samuel L. Jackson"]}`

Así como obtenemos todas estás ventajas, con la notación record tenemos la desventaja de que escribimos más. Uno tiene que evaluar cuándo vale la pena y cuándo no.

Múltiples constructores por tipo
--------------------------------

Nos interesa saber la densidad de un cuerpo. Por ahora vamos a manejar cilindros (de los cuales sabemos su masa, su altura y el radio de su base), cubos (sólo conocemos su masa y el largo de alguno de sus lados) y esferas (de ellas se conoce su masa y su radio).

Para calcular la densidad de un cuerpo vamos a utilizar la siguiente fórmula

`densidad = masa / volumen`
`data Cuerpo =`
` Cilindro`
`   {masa :: Float,`
`   altura :: Float,`
`   radio :: Float} |`
` Cubo`
`   {masa :: Float,`
`   lado :: Float} |`
` Esfera`
`   {masa :: Float,`
`   radio :: Float}`
` deriving (Show,Eq)`

Podemos ver que el tipo Cuerpo incluye los constructores Cilindro, Cubo y Esfera. Como la fórmula de la densidad es igual para todos los cuerpos podemos escribir:

`densidad unCuerpo = masa unCuerpo / volumen unCuerpo`

Ahora bien, el cálculo del volumen es algo particular para cada cuerpo

`volumen (Cilindro _ unaAltura unRadio) = pi * unRadio * unaAltura`
`volumen (Cubo _ unLado) = unLado ** 3`
`volumen (Esfera _ unRadio) = 4/3 * pi * (unRadio ** 3)`

Instance
--------

Ya dijimos que a cada restricción se la conoce como Clase/Class. A cada tipo que pertenece a una Clase se lo conoce como Instancia/Instance.

Por ejemplo la clase Eq en algún lugar del Prelude (la biblioteca standard de Haskell) puede estar definida así:

`-- Esto ya viene con Haskell`
`class Eq a where`
`  (==), (/=) :: a -> a -> Bool`
`-- Las instancias de Eq deben definir al menos una de estas 2 operaciones`
`  (/=) x y =  not (x == y)`
`  (==) x y =  not (x /= y)`

Si decimos que el tipo Bool pertenece a la clase Eq escribimos

`-- Esto ya viene con Haskell`
`instance Eq Bool where`
`  (==) True True = True`
`  (==) False False = True`
`  (==) _ _ = False`

Otro ejemplo con la clase Ord

`-- Notar que a tiene la restricción Eq en la definición de la clase Ord a`
`class Eq a => Ord a where`
` (<) :: a -> a -> Bool`
` (<=) :: a -> a -> Bool`
` (>) :: a -> a -> Bool`
` (>=) :: a -> a -> Bool`
` max :: a -> a -> a`
` min :: a -> a -> a`

Si queremos hacer que el TipoPelicula sea instancia de la clase Ord (por poner un ejemplo, definimos la función (&gt;) para que nos diga que una película es mayor que otra si su promedio de puntajes es mayor), podemos escribir:

`instance Ord TipoPelicula where`
`  (>) unaPelicula otraPelicula = promedio (puntajesPelicula unaPelicula) > promedio (puntajesPelicula otraPelicula)`
