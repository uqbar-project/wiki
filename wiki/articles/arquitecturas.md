Un artículo interesante de fowler: [1](http://martinfowler.com/bliki/DatabaseThaw.html)

### Clasificación general de las partes de una aplicación

Normalmente uno habla de tres capas, pero hay distintas formas de dividir y las divisiones principales son 4 que las gente agrupa de una u otra manera según distintas escuelas:

Presentación
Persistencia
Dominio
Aplicación  

### Transformación de los datos de entrada para hacerlos llegar hasta el dominio

Que la presentación no conozca al dominio
Que el dominio no conozca a la presentación  

Hay un lugar intermedio de almacenamiento:

Wrapper del request
Volcar los datos del request en un mapa
Volcar los datos en un dto (bean)  

### Estructura oficial de una aplicación

-   El struts llena un bean o mapa.
-   El controller (action de struts) recibe ese bean, invoca a un servicio.
-   El servicio es la fachada de la aplicación (facade) y esconde y proteje a los objetos de negocio

` * Permite tener muchas interfaces de usuario sobre el mismo dominio`
` * Permite interponer comportamiento genérico como manejo de transacciones y seguridad y remoticidad.`
` * Maneja la persistencia`
` * Provee una interfaz simplificada de la aplicación`
` * Decide qué "casos de uso" publicar y qué cosas no`
