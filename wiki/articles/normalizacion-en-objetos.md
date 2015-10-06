Objetivo
--------

El proceso de normalización se origina con el esquema relacional y ha sido ampliamente estudiado y difundido, ya que los RDBMS surgieron como una alternativa a los motores de bases de datos jerárquicos que permitían redundancia de la información y tenían problemas de consistencia, lo que llevaba a tener datos faltantes o duplicados.

Si bien el modelo de objetos tiene algunas características diferenciales respecto al relacional, podemos encontrar decisiones que tienen que ver con la aplicación (o no) de la normalización y el almacenamiento redundante de la información.

### Ejemplo

Consideraremos como ejemplo un dominio conocido: la relación many-to-many entre alumnos y cursos. Un alumno se inscribe en varios cursos y en cada curso tenemos muchos alumnos.

![](EjemploCursosAlumnos.png "EjemploCursosAlumnos.png")

### Recordemos qué busca la normalización

-   Evitar redundancias
-   Evitar inconsistencias: no quiero que un profesor renuncie y eso deje el curso apuntando a un profesor inexistente
-   Reducir el impacto de los cambios en los datos: si cargué mal la información de un profesor, debería actualizarlo en un solo lugar

Primer modelo posible
---------------------

El alumno tiene como atributos nombre, y los cursos. El curso tiene el nombre del profesor (un String) y los alumnos que participan.

Aplicando reglas de normalización (o no)
----------------------------------------

### Campos calculados

Es una técnica usual en muchas tecnologías, en objetos también. Podríamos pensar ejemplos:

-   la cantidad de alumnos de un curso
-   la cantidad de alumnos de un curso en condiciones de firmar
-   el promedio de notas de un alumno en un curso

se trata de atributos que pueden calcularse pero que por algún motivo elegimos almacenarlos como dato, ya sea

-   porque es conveniente cuando lo migramos a un esquema relacional, para facilitar los queries posteriores, ej: conocer los cursos con más de 40 alumnos sería

<code lang="sql">

`select *`
`  from cursos c`
` where c.cantidad_alumnos > 40`

</code>

Mientras que si no estuviera ese dato necesitamos hacer un join con la tabla de relación cursos-alumnos + el correspondiente count.

-   también se puede tratar de mejorar la performance, aún en objetos, en especial cuando es más frecuente consultar la cantidad de alumnos en un curso vs. inscribir un alumno a un curso

### Campos calculados vs. datos del negocio no siempre calculables

Si necesitamos saber cuántos inscriptos hubo al comienzo del cuatrimestre, debemos tener en cuenta que

-   el campo se puede calcular en un momento t<sub>0</sub>
-   pero una vez pasado ese momento, el cálculo se pierde

Este requerimiento **no tiene nada que ver con la normalización, porque no hay redundancia**, en ese caso lo que tenemos que hacer es crear un atributo en el Curso...

![](Curso-cantidadInscriptos.png "Curso-cantidadInscriptos.png")

### 1FN: Aplicabilidad en objetos

La primera forma normal nos pide que

-   **no haya filas duplicadas**, y esto se da mediante la identificación de claves candidatas
-   **no haya campos repetitivos / atributos multivaluados**

Aquí vemos que las restricciones de primera forma normal no aplican para el modelo de objetos, dado que no existe el concepto de relación o tabla como punto de concentración de todos los alumnos. Cada alumno que se crea forma parte del ambiente mientras tenga una referencia, y no hay riesgo de "filas duplicadas" ni necesidad de usar una clave candidata, ya que cada objeto nuevo tiene su propia identidad respecto a los demás objetos.

Por otra parte, un alumno puede tener una colección de cursos y cada curso una colección de alumnos (o un mapa alumno-notas). La restricción de no tener atributos multivaluados, o un atributo subdivisible en una estructura interna no aplica tampoco al modelo de objetos, donde la referencia es a cualquier tipo de objeto, incluido una colección.

### 2FN y 3FN en objetos

Tanto 2 como 3FN buscan que todo determinante sea clave candidata, o explicado en una manera más simple, no haya dependencias de ningún atributo con otro atributo

-   que forme parte de la clave principal
-   que no forme parte de la clave principal

Dado que en objetos no utilizamos el concepto de clave primaria, no tiene sentido discriminar cada caso en particular. Lo que sí podemos revisar es el ejemplo nuevamente, donde en el objeto Curso se registra la información sobre el legajoDocente (un entero) y el nombreDocente (un String).

![](ObjetoCursoDesnormalizado.png "fig:ObjetoCursoDesnormalizado.png") pasa a... ![](ObjetoCursoNormalizado.png "fig:ObjetoCursoNormalizado.png")

Nosotros podemos llegar a encontrar una abstracción Docente de dos maneras posibles:

-   aplicando la lógica de normalización, donde vemos que existe una dependencia funcional entre el nombre del docente y su legajo
-   o bien mucho antes, cuando necesitamos la abstracción Docente, porque es necesario agregarle **comportamiento**

*El proceso de normalización de entidades en el esquema relacional surge de la misma manera como un proceso de generación de abstracciones posibles en el modelo de objetos*.

Por último, podríamos decidir que nuestro objeto Curso tuviera los atributos docente (una referencia a un objeto Docente), legajoDocente y nombreDocente por dos motivos:

-   uno de negocio, si como en el caso anterior necesitáramos almacenar la información del docente que tomó el curso originalmente (y queremos mantenerla)
-   para mejorar la performance, en ese caso introducimos una redundancia desnormalizando la información del curso. Eso permite que podamos obtener la información de un curso sin necesidad de navegar hacia otras entidades, algo que todo diseñador debe contemplar para los casos de uso que el negocio exige.

### Redundancias por problemas de navegabilidad

El modelo relacional es sumamente flexible, en una relación many-to-many Alumno-Curso, podemos navegar la relación partiendo del curso o bien desde el alumno. Si necesitamos resolver estos requerimientos

-   saber qué alumnos del curso de Diseño aprobaron el primer parcial
-   saber en cuántos cursos está anotado un alumno de lunes a viernes

sabemos que es mucho más fácil si la relación de asociación entre Alumno y Curso es bidireccional, es decir que un alumno conoce la lista de cursos en los que está inscripto y un curso conoce la lista de alumnos que forman parte.

Resumen de diferencias entre el modelo relacional y el de objetos
-----------------------------------------------------------------

| Modelo relacional                                                                                       | Objetos                                                                                                                               |
|---------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------|
| Elimina duplicados mediante la primary key                                                              | Trabaja con identidad, no necesita claves naturales ni subrogadas                                                                     |
| No permite atributos multivaluados                                                                      | Permite referenciar a cualquier tipo de objetos, incluido conjuntos y mapas                                                           |
| Es un modelo flexible para navegar en cualquier dirección                                               | Las referencias tienen una sola dirección, para tener una relación bidireccional es necesario utilizar otra referencia                |
| Puedo manipular los datos, los triggers o constraints me permiten asegurar la consistencia de los datos | No accedo directamente a los datos sino que envío mensajes, y los métodos permiten asegurar la consistencia del estado de cada objeto |


