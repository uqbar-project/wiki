Highligths
----------

-   Stateful Model
    -   Manejo automático en sesión
    -   no requiere conocer scopes: session, request, etc.
-   Modelo de componentes al estilo SWT (Arena) que soporta extensiones utilizando las técnicas OO conocidas
-   Transparencia en la comunicación con el server
    -   Al igual que en cliente pesado, los métodos "onClick()" son invocados automáticamente por el framework
-   Html markup muy poco intrusivo
    -   Solo hay que anotar el tag con el id del componente. **Ej:** <a wicket:id="aceptar" href="#">`Aceptar`</a>

Problemas
---------

-   Sigue atado al concepto de formulario

`         o Si bien el form es un objeto con las ventajas que eso trae,`
`         o el desarrollador tiene que pensar en el submiteo del form. `
`   * Muy fuerte vínculo entre markup y modelo componentes java.`
`         o Hace rígida y burocrática la vista. `
`   * Modelo de componentes orientado a la jerarquía.`
`         o Hay que subclasear TODO!`
`         o Trabaja poco con composicion. `
`   * El uso de Model's (la M del MVC) es algo oscuro`
`         o no muy intuitivo.`
`         o no fomenta su uso por estas mismas complicaciones. `
`   * No parece ser posible autogenerar la vista (html) automaticamente (al menos en forma facil). Por ejemplo para una aplicacion/producto bien grande.`
