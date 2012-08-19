Concepto
========

-   <http://c2.com/cgi/wiki?AbstractDataType>
-   <http://es.wikipedia.org/wiki/Tipo_de_dato_abstracto>

Implementaciones de TADs
========================

En C
----

-   Definir si se lo pasará por valor o por referencia
-   Codificar el .h con las declaraciones de las funciones
-   Si usa memoria dinámica, definir una operación para la destrucción del TAD
-   Definir la estructura a emplear. Si lo hacemos en el .c o en el .h depende del grado de desacomplamiento que busquemos y nivel de paranoia que tengamos. Tambien hace mas compleja la implementación
-   Definir sinónimos de tipos para la estructura.
-   Implementar las funciones en .c

### Ejemplo: Un búffer

Un búffer es una estructura de datos similar a una cola, diseñada para permitir el agregado de elementos o lotes de elementos homogeneos de forma eficiente, al final del mismom controlando su capacidad y taza de expansión para reducir al mínimo las operaciones de gestión de memoria. A diferencia de una cola, los elementos del buffer no se sacan de a uno, sino que se extraen también en lotes. Buffers más avanzados soportan también punteros internos para delimitar regiones dentro del mismo.

Por sus características, los buffers son usados típicamente para almacenar bytes o caracteres.

Modelaremos un buffer de caracteres. Este soportará las siguientes operaciones:

-   Creación: se creará con un tamaño inicial fijado por el usuario
-   Concatenación: ofrecerá una operación para agregar un solo caracter, y otra para agregar un conjunto de caracteres.
-   Extracción: expondrá una operación para extraer todos los caracteres como un char\*. Una vez devuelto, es importante que el buffer pierda toda referencia al array original
-   Destrucción: expondrá una operación para destruir el buffer. Es importante que libere no solo la memoria usada por el buffer, sino también, la memoria del contenido del mismo, si no fue ya devuelto.

#### Declaración de las operaciones

Pensar las operaciones no se trata tan solo de pensar que funcionalidad expondrá, sino también, y en particular cuando tenemos TADs con estado mutable, pensar cuales son las precondiciones y postcondiciones de las mismas, y las invariantes del TAD.

Esta no es solo una buena práctica de diseño estructurado, sino que puede ser muy beneficiosa a la hora de trabajar en cualquier paradigma. Es decir, para especificar correctamente una operación no basta con indicar su firma/tipo y su semántica, sino también sus restricciones operacionales.

Estas declaraciones las colocaremos en un .h

`Buffer * buffer_new(int max_size);`
`void buffer_append_char(Buffer * self, char aChar);`
`void buffer_append_chars(Buffer * self, char * chars, int count);`
`char * buffer_extract(Buffer * self);`
`int  buffer_current_size(Buffer * self);`
`void buffer_delete(Buffer ** self);`

#### Definición de la estructura interna

#### Definición de las operaciones

En Haskell
----------

Ejemplo: una red neuronal
