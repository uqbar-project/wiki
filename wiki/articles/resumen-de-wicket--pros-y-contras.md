Resumen general
---------------

| Tema                                | Cómo lo resuelve Wicket                                                                                                                                                                                                                                                                          |
|-------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Widgets                             | Si bien dependen de lo que HTML ofrece, del lado del servidor se puede trabajar con componentes visuales que rendericen tablas paginadas, combos anidados, etc.                                                                                                                                  |
| Layout                              | Definido por el html. Separación entre html y java. Comparte las características de layout con web respecto al acomodamiento de los controles con FlowLayout, RowLayout, etc.                                                                                                                    |
| Binding                             | Se mapea cada componente con su modelo correspondiente, al igual que Arena. El CompoundPropertyModel trabaja por convención.                                                                                                                                                                     |
| Manejo de estado                    | La página tiene estado como cualquier objeto que pertenece al ambiente Java.                                                                                                                                                                                                                     |
| Navegación                          | Se pueden generar nuevas instancias de una página o recibir una página como parámetro. Al abrir nuevas sesiones desde el browser se generan nuevas instancias de página automáticamente. La navegación es a nivel aplicación y no hipervínculo de documento.                                     |
| Pasaje de información entre páginas | Puedo definir mis propios constructores para las páginas, pasando como parámetros todas las referencias a objetos que necesite (incluyendo la página padre).                                                                                                                                     |
| Manejo de eventos                   | Los botones deben definirse como submit y no como button para que pueda funcionar el binding de atributos, por las restricciones de la tecnología web: sólo con submit los parámetros no viajan al servidor. Tanto los buttons como los links definen sus listeners al mismo estilo SWT o swing. |

Highligths
----------

-   Stateful Model
    -   manejo automático de estado en sesión
    -   no requiere conocer scopes: session, request, etc.
-   Modelo de componentes al estilo [SWT](jface--controles-y-binding.html)
    -   soporta extensiones trabajando con técnicas OO conocidas
-   Transparencia en la comunicación con el server
    -   Al igual que en cliente pesado, los métodos "onClick()" son invocados automáticamente por el framework
-   Html markup muy poco intrusivo
    -   Sólo hay que anotar el tag con el id del componente. Ej: <a wicket:id="aceptar" href="#">`Aceptar`</a>
    -   Al trabajar con un html de prueba, eso permite la separación de tareas entre el diseñador y el programador (el diseñador no sufre si hay problemas de performance o cambio en los datos porque el html contiene un prototipo de ejemplo en sí mismo)

Problemas
---------

-   Sigue atado al concepto de formulario
    -   Si bien el form es un objeto con las ventajas que eso trae,
    -   el desarrollador tiene que pensar en el submiteo del form.
-   Muy fuerte vínculo entre markup y modelo componentes java.
    -   Hace rígida y burocrática la vista.
-   Modelo de componentes orientado a la jerarquía
    -   Hay que subclasear ¡TODO! (botones, links, etc.)
    -   Trabaja poco con composición
    -   Una vez que el programador se hizo amigo de la tecnología, se imponen refactors para evitar escribir muchas líneas de código (que tiende a ser repetitivo, podría mejorarse la interfaz de creación de controles del lado del servidor)
-   El uso de Model's (la M del MVC) es algo oscuro
    -   no muy intuitivo
    -   no fomenta su uso por estas mismas complicaciones.
-   No parece ser posible autogenerar la vista (html) automáticamente (al menos en forma fácil). Por ejemplo para una aplicación/producto grande, donde se necesite desarrollar muchas pantallas de carga.

Links relacionados
------------------

-   [Algo3 Temario](algo3-temario.html)

