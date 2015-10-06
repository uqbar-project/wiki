El proceso de normalización se origina con el esquema relacional y ha sido ampliamente estudiado y difundido, ya que los RDBMS surgieron como una alternativa a los motores de bases de datos jerárquicos que permitían redundancia de la información y tenían problemas de consistencia, lo que llevaba a tener datos faltantes o duplicados.

Si bien el modelo de objetos tiene algunas características diferenciales respecto al relacional, podemos encontrar decisiones que tienen que ver con la aplicación (o no) de la normalización y el almacenamiento redundante de la información.

Consideraremos como ejemplo un dominio conocido: la relación many-to-many entre alumnos y cursos. Un alumno se inscribe en varios cursos y en cada curso tenemos muchos alumnos.
