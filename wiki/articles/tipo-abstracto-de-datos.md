---
layout: article
title: Tipo abstracto de datos
---

# Concepto

-   <http://c2.com/cgi/wiki?AbstractDataType>
-   <http://es.wikipedia.org/wiki/Tipo_de_dato_abstracto>

# Implementaciones de TADs


## En C

-   Definir si se lo pasará por valor o por referencia
-   Codificar el `.h` con las declaraciones de las funciones
-   Si usa memoria dinámica, definir una operación para la destrucción del TAD
-   Definir la estructura a emplear. Si lo hacemos en el `.c` o en el `.h` depende del grado de desacoplamiento que busquemos y nivel de paranoia que tengamos. Tambien hace mas compleja la implementación
-   Definir sinónimos de tipos para la estructura.
-   Implementar las funciones en `.c`

### Convenciones

Si bien para implementar un TAD no es necesario seguir ninguna convención en particular, dado que C no tiene un soporte nativo para los mismos, y por tanto hay varias formas de implementarlos, es recomendable seguir convenciones para mantener consistencia en sus implemenentaciones y facilitar la comprensión del código a los demás programadores (o a uno mismo en el futuro). Nosotros aquí sugerimos la siguiente:

-   Escribir los tipos en CamelCase. Por ejemplo *AerolineaLanchita*
-   Escribir las operaciones en snake\_case.
-   Dado que nuestros TADs no presentarán polimorfismo (al menos no el los ejemplos básicos), las operaciones estarán prefijadas por el tipo de dato, en snake case. Esto se hará así para evitar colisiones en el espacio de nombres, y facilitar el autocompletado por parte del IDE. Por ejemplo *areolinea\_lanchita\_comprar(char \* codigo\_pasaje)*
-   Las operaciones privadas del TAD las declararemos y definiremos dentro del .c, prefijandolas con guión bajo, y declarandolas static
-   Respetaremos todas las buenas prácticas de programación que ya conocemos de paradigmas: delegación, expresividad, declaratividad (hasta donde podamos)

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

```C
Buffer * buffer_new(int max_size);
void buffer_append_char(Buffer * self, char aChar);
void buffer_append_chars(Buffer * self, char * chars, int count);
char * buffer_extract(Buffer * self);
int  buffer_current_size(Buffer * self);
void buffer_delete(Buffer ** self);
```

#### Definición de la estructura interna

*Para mas detalles sobre la sintaxis, ver [Typedefs y tipos anónimos](typedefs-y-tipos-anonimos.html)*

Para modelar la estructura interna del buffer, utilizaremos un *struct*, que tendrá un campo por cada atributo de nuestro TAD: contenido, tamaño máximo y tamaño actual.

```C
typedef struct {
  char * content;
  int current_size;
  int max_size;
} Buffer;
```

#### Definición de las operaciones

*Para mas detalles sobre las funciones de manejo de memoria, ver [Manejo de memoria en C](manejo-de-memoria-en-c.html)*

La primera operación que debemos soportar es la instanciación del TAD. Esta operación debe tomar un tamaño máximo inicial, y reservar la memoria necesaria para el buffer en sí mismo, y el vector de memoria donde se copiarán los contenidos. También debe inicializar los atributos del TAD.

Por convención y analogía con objetos, llamaremos a esta operación **new**:

```C
Buffer * buffer_new(int max_size) {
  Buffer * self = instance_new(Buffer);
  self->content = instance_new_array(char, max_size);
  self->current_size = 0;
  self->max_size = max_size;
  return self;
}
```

La contrapartida de la instanciación del TAD es la destrucción del mismo. En ambientes con Garbage Collector esto no es normalmente necesario, por lo que no existe operación análoga en objetos. Por convención, la llamaremos **delete**:

```C
void buffer_delete(Buffer ** self) {
  if( (*self)->content != NULL ) {
    instance_delete((*self)->content);
  }
  instance_delete(*self);
}
```

Nótese que esta operación toma como argumento una doble indirección a un Buffer.

En este caso particular, el enunciado nos plantea que solo se debe liberar la memoria del contenido del buffer, si y solo si no se ha devuelto el contenido al usuario del TAD. Para resolver esto, señalizaremos un contenido entregado al usuario setéandolo en NULL.

Esto mismo debemos considerarlo a la hora de justamente devolver ese contenido, mediante la operación **extract**. Para cumplir con la precondición de que el contenido no ha sido devuelto antes, agregaremos una aserción:

```C
char * buffer_extract(Buffer * self) {
  assert(self->content != NULL);
  buffer_append_char(self, '\0');
  char * content = self->content;
  self->content = NULL;
  return content;
}
```

Luego tenemos que encarar el problema de agregar caracteres. Llamaremos a esta operación *'apend\_chars*, que tomará como parámetro el vector de caracteres a copiar, y su longitud.

La operación no es trivial: debemos considerar que si no hay suficiente espacio en el buffer (es decir, la diferencia entre el tamaño actual y el máximo es menor a la longitud del vector), deberemos redimesionar el buffer de forma eficiente.

Sin embargo, aún así podemos comenzar a atacar el problema, delegando apropiadamente:

```C
void buffer_append_chars(Buffer * self, char * chars, int count){
  _buffer_expand(self, count);
  memcpy(self->content + self->current_size, chars, count);
  self->current_size += count;
}
```

Es decir, decimos que para copiar un vector de caracteres al final del buffer, debemos expandir nuestro buffer tanto como sea necesario para almacenar los caracteres, y luego procedemos a efectivamente realizar la copia y actualizar el tamaño actual. Ahora, tenemos un problema un poco mas simple: solo debemos preocuparnos por redimensionar el buffer, de ser necesario.

Dado que **\_buffer\_expand** no será usada desde el exterior (es una operación interna), no la colocamos en nuestro .h.
