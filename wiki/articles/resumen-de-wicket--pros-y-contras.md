Highligths
----------

-   Stateful Model
    -   manejo automático de estado en sesión
    -   no requiere conocer scopes: session, request, etc.
-   Modelo de componentes al estilo [JFace: Controles y binding](swt.html)
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

