---
layout: article
title: Data  definiendo nuestros tipos en haskell
---

# Data: qué es y para qué sirve

Usamos data para definir un nuevo tipo de dato, por ejemplo si quisiéramos definir el tipo Booleano escribiríamos

```Haskell
data Booleano = <...> 
```

Qué vamos a escribir en <...>? los posibles valores que tiene ese tipo separados por un pipe (|)

```Haskell
data Booleano = Falso | Verdadero
```

Con esta notación podríamos pensar que el tipo Int está escrito de la siguiente manera

```Haskell
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647  
```

**Nota: Int no está definido de esta manera pero sirve para entender el uso de data**

Si quisiéramos escribir el tipo ColorPrimario cuales serían sus posibles valores?

```Haskell
data ColorPrimario = Rojo | Amarillo | Azul
```

Si luego quisiéramos hacer una función combinar que reciba dos colores primarios distintos y me retorne un color secundario (Naranja, Violeta o Verde), podemos resolverlo usando [pattern matching](pattern-matching-en-haskell.html):

```Haskell
data ColorSecundario = Naranja | Violeta | Verde 

combinar :: ColorPrimario -> ColorPrimario -> ColorSecundario
combinar Rojo Amarillo = Naranja
combinar Amarillo Rojo = Naranja
combinar Rojo Azul = Violeta
combinar Azul Rojo = Violeta
combinar Amarillo Azul = Verde
combinar Azul Amarillo = Verde
```

Cabe mencionar que esto mismo de los colores lo podríamos haber hecho con Strings, pero haberlo hecho con data limita los valores posibles a aquellas cosas que consideramos válidas para el tipo específico que queremos representar.

## Tuplas tuplas tuplas

Una forma de representar valores que están compuestos por otros valores, tal que cada uno de ellos tiene una semántica distinta, es usando uno de los [tipos compuestos bien conocidos de Haskell, las tuplas](tipos-de-haskell.html).

Vamos a hacer un ejemplo con tuplas de 2 elementos por simplicidad, pero lo mismo se aplica para tuplas de n elementos. Supongamos que queremos armar un programa en el cual queremos representar un alumno por su nombre (un String) y sus notas (una lista de Int = \[Int\]) y también queremos representar una película por su título (unString) y los puntajes que le ponen los críticos en imdb (una lista de Int = \[Int\]). Por ejemplo:

```Haskell
cursoK9 = [("Federico",[2,3]),("Líder",[10,10,10,10,10]),("Germain",[8,9,10])]
pelis = [("Pedornia", [0,0,-3,-666]),("Pulp Fiction", [9,10,9]),("Fight Club", [3,8,8,9,9,10])]
```

Vamos a agregar además algunas funciones básicas para interactuar con alumnos y películas fácilmente:

```Haskell
nombreAlumno unAlumno = fst unAlumno
notasAlumno unAlumno = snd unAlumno
tituloPelicula unaPelicula = fst unaPelicula
puntajesPelicula unaPelicula = snd unaPelicula
```

Y una de las cosas que queremos hacer con los alumnos es saber si empezó mal (si su primer nota no está aprobada):

```Haskell
empezoMal = not . aprobada . head . notasAlumno
aprobada nota = nota >= 6
```

Si usamos lo que definimos arriba como un solo programa (un solo .hs), podemos ver que:

-   La función nombreAlumno es igual a la función tituloPelicula
-   La función notasAlumno es igual a la función puntajesPelicula

Nada me impide consultar:

```Haskell
> puntajesPelicula ("Líder",[10,10,10,10,10])
[10,10,10,10,10]

> empezoMal (head pelis)
True
```

Todo esto es posible porque si miramos los [tipos que infiere Haskell](inferencia-de-tipos.html) no existe diferencia entre una película y un alumno, para Haskell los alumnos y películas son sólo tuplas de 2 elementos. Ejemplo:

```Haskell
> puntajesPelicula ([1,2,3],(True,"hola"))
(True,"hola")
```

Si definimos un [alias de tipo](tipos-de-haskell.html) para Alumno y Pelicula de esta forma:
```Haskell
type Alumno = (String, [Int])
type Pelicula = (String, [Int])
```

Y luego restringimos los tipos de todas las funciones para explicitar que lo que reciben son alumnos o películas según corresponda:
```Haskell
nombreAlumno :: Alumno -> String
nombreAlumno unAlumno = fst unAlumno
notasAlumno :: Alumno -> [Int]
notasAlumno unAlumno = snd unAlumno

tituloPelicula :: Pelicula -> String
tituloPelicula unaPelicula = fst unaPelicula
puntajesPelicula :: Pelicula -> [Int]
puntajesPelicula unaPelicula = snd unaPelicula

empezoMal :: Alumno -> Bool
empezoMal = not . aprobada . head . notasAlumno
```

De lo único que nos salvamos es de consultas como:
```Haskell
> puntajesPelicula ([1,2,3],(True,"hola"))
```
Porque esa tupla contiene elementos de tipos que no coinciden con los explicitados. Pero esta otra consulta no se ve afectada por el cambio que realizamos, porque tanto Alumno como Pelicula no son más que `(String, [Int])`:
```Haskell
> empezoMal (head pelis)
True
```

Ya que Haskell es un lenguaje "que se fija mucho en los tipos", nos gustaría que un caso como los de arriba nos tirará error (donde en vez de mandar un alumno o una película según corresponda, enviamos cualquier otra cosa incluyendo un alumno donde se esperaba una película y visceversa).

## Definiendo nuevos tipos

Para poder diferenciar a un alumno de una película y a ambos de una tupla, tenemos que definir un nuevo tipo. Eso se hace usando data:

```Haskell
data NuevoTipo = Constructor Tipo1 Tipo2 ... Tipon
```

**Nota: el tipo y el constructor pueden llamarse igual, usaremos nombres distintos a fines didácticos para remarcar en qué contextos lo que usamos es el constructor y en cuáles el tipo.**

En nuestro ejemplo:

```Haskell
data Alumno = UnAlumno String [Int]
data Pelicula = UnaPelicula String [Int]
```

Ahora, para obtener un nuevo alumno o una nueva película, tenemos que usar el "Constructor"

```Haskell
cursoK9 = [UnAlumno "Federico" [2,3], UnAlumno "Líder" [10,10,10,10,10], UnAlumno "Germain" [8,9,10]]
```

```Haskell
-- No cambia
empezoMal unAlumno = 4 > head (notasAlumno unoAlumno)
```

```Haskell
pelis = [UnaPelicula "Pedornia" [0,0,-3,-666], UnaPelicula "Pulp Fiction" [9,10,9], UnaPelicula "Fight Club" [8,8,8,9,9,10]]
```

```Haskell
-- Ahora estas funciones usan Pattern-Matching!
nombreAlumno (UnAlumno nombre notas) = nombre 
notasAlumno (UnAlumno nombre notas) = notas
tituloPelicula (UnaPelicula nombre notas) = nombre
puntajesPelicula (UnaPelicula nombre notas) = notas
```

Es importante remarcar que al hacer esto un alumno o una película **YA NO ES UNA TUPLA**

```Haskell
fst :: (a,b) -> a
nombreAlumno :: Alumno -> String
tituloPelicula :: Pelicula -> String
snd :: (a,b) -> b
notasAlumno :: Alumno -> [Int]
puntajesPelicula :: Pelicula -> [Int]
```

```Haskell
cursoK9 :: [ Alumno ]
empezoMal :: Alumno -> Bool
```

```Haskell
pelis :: [ Pelicula ]
```

Ejemplos:

A partir de estos valores:
```Haskell
fede = UnAlumno "Federico" [2,3]
ger = UnAlumno "Germain" [8,9,10]
pulp = UnaPelicula "Pulp Fiction" [9,10,9]
```

Veamos qué sucede al hacer algunas consultas sobre funciones que esperan tuplas, alumnos o películas.
```Haskell
> fst fede
Error (fst espera una tupla y fede es de tipo Alumno)
> nombreAlumno fede
"Federico"
> nombreAlumno pulp
Error (nombreAlumno espera algo de tipo Alumno y pulp es de tipo Pelicula)
> puntajesPelicula fede
Error (puntajesPelicula espera Pelicula y fede es de tipo Alumno)
> puntajesPelicula pulp
[9,10,9]
> empezoMal fede
True
> empezoMal (head pelis)
Error (empezoMal espera Alumno y el primer elemento de pelis es de tipo Pelicula)
```

## Derivar typeclasses

Es muy común querer comparar por igualdad y mostrar por pantalla un valor que tiene un tipo definido por nosotros.

```Haskell
> head cursoK9
Error (Alumno no tiene la restricción Show)
```

Para que esto funcione deberíamos:

-   Decir que Alumno es un tipo que pertenece a la restricción [Show](typeclasses.html)
-   Definir la función show para un Alumno

En vez de hacer esto a mano (agregando una instancia de la typeclass como se explica más adelante), y gracias a que los elementos que forman un Alumno SI tienen la restricción Show, podemos hacer que el Alumno "derive" esa restricción

```Haskell
--Lo único que hay que agregar es deriving (Show)
data Alumno = UnAlumno String [Int] deriving (Show)
```

Con este agregado podemos hacer

```Haskell
> head cursoK9
UnAlumno "Federico" [2,3]
```

Ahora, si hacemos lo siguiente

```Haskell
> fede == ger
Error (el Alumno no tiene la restricción Eq)
```

También parece común querer preguntar si dos alumnos son iguales (o distintos), pasa lo mismo que con Show, nos gustaría que el Alumno pertenezca a la typeclass Eq.

```Haskell
--Lo único que hay que agregar es deriving (Show,Eq)
data Alumno = UnAlumno String [Int] deriving (Show,Eq)
```

Con este agregado podemos hacer:

```Haskell
> fede == ger
False
> UnAlumno "Roberto" [7,8,9] == UnAlumno "Huberto" [7,8,9]
False
> UnAlumno "Roberto" [7,8,9] == UnAlumno "Roberto" [7,8,9]
True
```

También se puede utilizar el deriving con la clase Ord

```Haskell
data Nota = Insuficiente | Regular | Bien | MuyBien
```

Al hacer:

```Haskell
Main> Insuficiente > Regular
ERROR: No instance for (Ord Nota)
```

Esto se debe a que el tipo Nota no cumple con la restricción Ord, por defecto se considera a los valores en forma ascendente de izquierda a derecha (i.e. Insuficiente < Regular < Bien < MuyBien).

Para obtener este comportamiento en los valores del tipo Nota lo único que debemos hacer es "derivar" la restricción Ord

```Haskell
data Nota = Insuficiente | Regular | Bien | MuyBien deriving Ord
```

```Haskell
Main> Insuficiente > Regular
False
```

Lo mismo podría hacerse con los tipos ColorPrimario y ColorSecundario definidos anteriormente en este artículo. De seguro vamos a querer que puedan mostrarse. Con derivar Show para ColorSecundario sería suficiente para poder usar la función `combinar :: ColorPrimario -> ColorPrimario -> ColorSecundario` desde la consola y ver el resultado, pero a su vez poder ver los colores primarios suena como algo deseable.

También podemos sacarle provecho a derivar Eq, lo que nos permitirá llegar a esta nueva solución sin repetición de lógica:
```Haskell
data ColorPrimario = Rojo | Amarillo | Azul deriving (Show, Eq)
data ColorSecundario = Naranja | Violeta | Verde deriving (Show, Eq)

combinar :: ColorPrimario -> ColorPrimario -> ColorSecundario
combinar Rojo Amarillo = Naranja
combinar Rojo Azul = Violeta
combinar Amarillo Azul = Verde
combinar color1 color2 | color1 /= color2 = combinar color2 color1
```

# Data con Record Syntax

Es muy común hacer funciones para obtener los valores que forman nuestro individuo compuesto como hicimos con las películas y los alumnos.

Imaginen que ahora queremos agregarle a nuestro tipo Pelicula (además del nombre y sus puntajes), el nombre del director, el nombre de los actores principales y el año en que se estrenó.

```Haskell
data Pelicula = UnaPelicula String String [String] Int [Int]
```

Lo primero que notamos es que no es tan fácil identificar cada elemento. Para eso existe la posibilidad de declarar sinónimos de tipo usando [type](tipos-de-haskell.html). En el ejemplo de las películas podemos hacer algo como:

```Haskell
type Titulo = String
type NombreDirector = String
type Puntajes = [Int]

data Pelicula = UnaPelicula Titulo NombreDirector [String] Int Puntajes deriving (Show,Eq)

narnia = UnaPelicula "Pedornia" "Andrew Adamson" ["Tilda Swinton", "Georgie Henley","William Moseley"] 2005 [0,0,-3,-666]
pulp = UnaPelicula "Pulp Fiction" "Quentin Tarantino" ["John Travolta", "Uma Thurman", "Samuel L. Jackson"] 1994 [9,10,9]
fc = UnaPelicula "Fight Club" "David Fincher" ["Brad Pitt", "Edward Norton", "Helena Bonham Carter"] 1999 [8,8,8,9,9,10]
```

Lo cual mejora un poco la expresividad de la definición. Igualmente a la hora de construir el dato tenemos que tener cuidado de no pasar primero el nombre del director y luego el título, porque al fin y al cabo los dos son de tipo String, y por ende va a tipar una construcción incorrecta en base a nuestro dominio.

Otro tema es que tenemos que definir nuevamente funciones como tituloPelicula y puntajesPelicula:

```Haskell
tituloPelicula (UnaPelicula nombre director actores anioEstreno notas ) = nombre
puntajesPelicula (UnaPelicula nombre director actores anioEstreno notas ) = notas
```

Como en cualquier otro programa, las variables que no nos interesan en absoluto pueden ser reemplazadas por la variable anónima

```Haskell
tituloPelicula (UnaPelicula nombre _ _ _ _ ) = nombre
puntajesPelicula (UnaPelicula _ _ _ _ notas ) = notas
```

Lógicamente también tenemos que definir funciones para el resto de los campos que antes no existían:
```Haskell
directorPelicula (UnaPelicula _ director _ _ _ ) = director
actores (UnaPelicula _ _ actores _ _ ) = actores
anioEstreno (UnaPelicula _ _ _ anio _ ) = anio
```

Una forma más rápida de definir este tipo de funciones es usando **la sintaxis de registro** (disponible en GHC, no en Hugs).

En vez de definir sólo los tipos de los valores que van a estar en la película, también agregamos en la definición el nombre de la función por el cual queremos obtener dicho valor. 

Al utilizar la notación de registro hay que encerrar la definición de los campos entre llaves { } y separar cada campo por comas.

```Haskell
data Pelicula = 
  UnaPelicula
    {tituloPelicula :: String ,
    directorPelicula :: String,
    actores :: [String],
    anioEstreno :: Int,
    puntajesPelicula :: [Int]}
    deriving (Show,Eq)
```

Con esta definición automaticamente Haskell define por nosotros las funciones tituloPelicula, puntajesPelicula, directorPelicula, actores y anioEstreno. El dominio de cada una de estas funciones es Pelicula y retornan lo que corresponda en cada caso.

Además cuando querramos obtener una nueva Película, podemos hacer

```Haskell
pulp = UnaPelicula "Pulp Fiction" "Quentin Tarantino" ["John Travolta", "Uma Thurman", "Samuel L. Jackson"] 1994 [9,10,9]
```

O bien podemos usar la siguiente notación que sólo es válida para datas definidos de esta forma. Como se puede ver, es más claro a que campo pertenece cada valor y no es necesario seguir un orden en los valores mientras se indique a que campo pertenece

```Haskell
pulp = 
  Pelicula{
    tituloPelicula = "Pulp Fiction",
    directorPelicula = "Quentin Tarantino",
    anioEstreno = 1994,
    puntajesPelicula = [9,10,9],
    actores = ["John Travolta", "Uma Thurman", "Samuel L. Jackson"]}
```

Esto ayuda mucho a la expresividad, pero también es más verboso. Uno tiene que evaluar cuándo vale la pena y cuándo no.

Otra cosa simpática de definir el data con sintaxis de registro es que que si el tipo deriva la typeclass Show, lo que se imprima en la consola cuando la expresión evaluada retorna algo de nuestro tipo (Pelicula en este caso) será más fácil de entender, porque mostrará cada valor asociado al nombre del campo en vez de uno al lado del otro, independientemente de qué notación se use para crear la película en cuestión.

# Cómo instanciar una typeclass

Ya dijimos que a cada restricción se la conoce como typeclass. A cada tipo que pertenece a una typeclass se le debe definir una instancia de la misma.

Por ejemplo la clase Eq en algún lugar del Prelude (la biblioteca standard de Haskell) puede estar definida así:

```Haskell
-- Esto ya viene con Haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool
-- Las instancias de Eq deben definir al menos una de estas 2 operaciones
  (/=) x y =  not (x == y)
  (==) x y =  not (x /= y)
```

Si decimos que el tipo Bool pertenece a la clase Eq escribimos

```Haskell
-- Esto ya viene con Haskell
instance Eq Bool where
  (==) True True = True
  (==) False False = True
  (==) _ _ = False
```

Otro ejemplo con la clase Ord

```Haskell
-- Notar que a tiene la restricción Eq en la definición de la clase Ord a
class Eq a => Ord a where
 (<) :: a -> a -> Bool
 (<=) :: a -> a -> Bool
 (>) :: a -> a -> Bool
 (>=) :: a -> a -> Bool
 max :: a -> a -> a
 min :: a -> a -> a
```

Si queremos hacer que el tipo Pelicula sea instancia de la clase Ord (por poner un ejemplo, definimos la función (>) para que nos diga que una película es mayor que otra si su promedio de puntajes es mayor), podemos escribir:

```Haskell
instance Ord Pelicula where
  (>) unaPelicula otraPelicula = promedio (puntajesPelicula unaPelicula) > promedio (puntajesPelicula otraPelicula)
```

Por lo general es suficiente con derivar typclasses en nuestros data, pero es importante recordar que para poder hacer esto, todos los componentes del data a su vez deben pertenecer al typeclass que estamos derivando. Y si en algún momento nos pasa que tenemos un data que se compone, entre otras cosas, por alguna función, ya no vamos a poder derivar así como así Show y Eq que son las más usuales, porque las funciones no son ni Show ni Eq.

En esos casos podemos o bien optar porque nuestro data no sea Show o Eq, o bien definir un instance para esta typeclass que se corresponda con nuestro tipo de dato y así determinar nuestra propia solución a ese problema.

# Múltiples constructores por tipo

Supongamos que nos interesa saber la densidad de un cuerpo. Por ahora vamos a manejar cilindros (de los cuales sabemos su masa, su altura y el radio de su base), cubos (sólo conocemos su masa y el largo de alguno de sus lados) y esferas (de ellas se conoce su masa y su radio).

Para calcular la densidad de un cuerpo vamos a utilizar la siguiente fórmula: densidad = masa / volumen.

Arranquemos por declarar el tipo de dato para representar un cuerpo:

```Haskell

data Cuerpo =
 Cilindro
   {masa :: Float,
   altura :: Float,
   radio :: Float} |
 Cubo
   {masa :: Float,
   lado :: Float} |
 Esfera
   {masa :: Float,
   radio :: Float}
 deriving (Show,Eq)
```

Podemos ver que el tipo Cuerpo incluye los constructores Cilindro, Cubo y Esfera. Como la fórmula de la densidad es igual para todos los cuerpos podemos escribir:

```Haskell
densidad unCuerpo = masa unCuerpo / volumen unCuerpo
```

Ahora bien, el cálculo del volumen es algo particular para cada cuerpo

```Haskell
volumen (Cilindro _ unaAltura unRadio) = pi * unRadio * unaAltura
volumen (Cubo _ unLado) = unLado ** 3
volumen (Esfera _ unRadio) = 4/3 * pi * (unRadio ** 3)
```

