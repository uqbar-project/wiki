El proceso de normalización se origina con el esquema relacional y ha sido ampliamente estudiado y difundido, ya que los RDBMS surgieron como una alternativa a los motores de bases de datos jerárquicos que permitían redundancia de la información y tenían problemas de consistencia, lo que llevaba a tener datos faltantes o duplicados.

Si bien el modelo de objetos tiene algunas características diferenciales respecto al relacional, podemos encontrar decisiones que tienen que ver con la aplicación (o no) de la normalización y el almacenamiento redundante de la información.

### Ejemplo

Consideraremos como ejemplo un dominio conocido: la relación many-to-many entre alumnos y cursos. Un alumno se inscribe en varios cursos y en cada curso tenemos muchos alumnos.

Recordemos qué busca la normalización
-------------------------------------

-   Evitar redundancias
-   Evitar inconsistencias: no quiero que un profesor renuncie y eso deje el curso apuntando a un profesor inexistente
-   Reducir el impacto de los cambios en los datos: si cargué mal la información de un profesor, debería actualizarlo en un solo lugar

Diferencias importantes a la hora de normalizar
-----------------------------------------------

| Modelo relacional                                                                                       | Objetos                                                                                                                               |
|---------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------|
| Elimina duplicados mediante la primary key                                                              | Trabaja con identidad, no necesita claves naturales ni subrogadas                                                                     |
| No permite atributos multivaluados                                                                      | Permite referenciar a cualquier tipo de objetos, incluido conjuntos y mapas                                                           |
| Es un modelo flexible para navegar en cualquier dirección                                               | Las referencias tienen una sola dirección, para tener una relación bidireccional es necesario utilizar otra referencia                |
| Puedo manipular los datos, los triggers o constraints me permiten asegurar la consistencia de los datos | No accedo directamente a los datos sino que envío mensajes, y los métodos permiten asegurar la consistencia del estado de cada objeto |

Primer modelo posible
---------------------

El alumno tiene como atributos nombre, y los cursos. El curso tiene el nombre del profesor (un String) y los alumnos que participan.

Aplicando reglas de normalización (o no)
----------------------------------------

### 1FN: Aplicabilidad en objetos

La primera forma normal nos pide que

-   **no haya filas duplicadas**, y esto se da mediante la identificación de claves candidatas
-   **no haya campos repetitivos / atributos multivaluados**

Aquí vemos que las restricciones de primera forma normal no aplican para el modelo de objetos, dado que no existe el concepto de relación o tabla como punto de concentración de todos los alumnos. Cada alumno que se crea forma parte del ambiente mientras tenga una referencia, y no hay riesgo de "filas duplicadas" ni necesidad de usar una clave candidata, ya que cada objeto nuevo tiene su propia identidad respecto a los demás objetos.

Por otra parte, un alumno puede tener una colección de cursos y cada curso una colección de alumnos (o un mapa, como veremos a continuación). La retricción de no tener atributos multivaluados, o un atributo subdivisible en una estructura interna no aplica tampoco al modelo de objetos, donde la referencia es a cualquier tipo de objeto, incluido una colección.

### Campos calculados

Es una técnica usual en muchas tecnologías, en objetos también. Podríamos pensar ejemplos:

-   la cantidad de alumnos de un curso
-   la cantidad de alumnos de un curso en condiciones de firmar
-   el promedio de notas de un alumno en un curso

Se trata de atributos que pueden calcularse pero que por algún motivo elegimos almacenarlos como dato, ya sea

-   porque es conveniente cuando lo migramos a un esquema relacional, para facilitar los queries posteriores, ej: conocer los cursos con más de 40 alumnos sería

<code lang="sql">

`select *`
`  from cursos c`
` where c.cantidad_alumnos > 40`

</code>

Mientras que si no estuviera ese dato necesitamos hacer un join con la tabla de relación cursos-alumnos + el correspondiente count.

-   también se puede tratar de mejorar la performance, aún en objetos, en especial cuando es más frecuente consultar la cantidad de alumnos en un curso vs. inscribir un alumno a un curso

