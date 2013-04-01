Programar implica muchas veces manejar grandes cantidades de información, un programa es una entidad muy compleja y por lo tanto es muy difícil abarcarlo en su totalidad. Por eso necesitamos herramientas que nos permitan manejar esa complejidad.

Gran parte de esas ideas trabajan con el principio de “dividir y conquistar”, esto es, tomar una parte más pequeña del programa y poder comprenderla olvidándonos momentáneamente del resto del programa. Sin embargo, siempre que uno tome una parte de un programa más grande, esa parte tendrá relaciones con otras partes del sistema. Para entenderlo yo debo poder incorporar esas otras partes a mi análisis pero sin tener que comprender todos sus detalles. Las herramientas que nos permiten eso las denominamos **abstracciones**.

Una abstracción es una forma de interpretar y conceptualizar lo que resulta más importante de una entidad compleja, sin tener que tener en cuenta todos sus detalles. Me permite quedarme con lo esencial descartando lo que (para mí, en ese momento) es accesorio.

Una abstracción es un concepto o una idea que no está asociado a ningún caso concreto. Abstraer es formar una idea general a partir de casos particulares. En la vida cotidiana usamos abstracciones todo el tiempo y gracias a eso, por ejemplo, podemos saber que una mesa es una mesa más allá de si es cuadrada o redonda, de madera o de plástico, con 4, 3 o 6 patas. Cuando programamos también es importante encontrar buenas abstracciones.

Cada uno de los paradigmas que vamos a aprender, nos va a brindar sus propias formas de abstracción. Las abstracciones de cada paradigma van a ser conceptos fundamentales en esta materia. Para los que vienen de programar estructurado, la forma de abstracción más conocida es el procedimiento. Un procedimiento permite tomar un conjunto de instrucciones y darles un nombre para poder utilizarla en otro contexto. Quien invoca el procedimiento se concentra en qué es lo que necesita resolver, el procedimiento es el que implementa el cómo se resuelve un determinado problema.

La [Declaratividad](declaratividad.html) es una forma de abstracción muy poderosa, que nos permite describir el conocimiento relativo a un problema desentendiéndonos de los algoritmos necesarios para manipular esa lógica, que son provistos por el motor.

Alto nivel y Bajo nivel
-----------------------

Muchas veces se habla de lenguajes de alto y bajo nivel, términos que se refieren al nivel de abstracción de cada lenguaje. Esta clasificación no es un blanco y negro, sino que sirve para comparar entre diferentes lenguajes respecto a qué tan cercanas son sus abstracciones a la máquina (bajo nivel) o al programador (alto nivel).

A Assembler (trabaja directamente con instrucciones de máquina y los registros de la computadora) podemos considerarlo como de más bajo nivel que C por ejemplo (que es un lenguaje que se compila a código de máquina y nos permite trabajar con procedimientos y estructuras complejas), sin embargo se puede ir más a bajo nivel que con Assembler (siempre hay unos y ceros más abajo) y más alto nivel que C (todos los lenguajes que usamos en la materia tienen estas características, un ejemplo de ello es que no necesitamos manejar nuestra propia memoria, existen mecanismos para ello sobre los cuales nos paramos).
