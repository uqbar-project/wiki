La migración desde trabajar con Ozono a pasar a la versión full de Pharo puede ser complicado en ciertos sentidos pero más o menos es lo mismo pero tenemos que tener en cuenta las diferencias.

Clases vs. Objetos
------------------

La primer diferencia que tenemos en este nuevo ambiente es que Pharo esta centrado en las clases y no en los objetos.

Como ya vimos en las clases teóricas, los objetos ya no los vamos a crear uno por uno sino que vamos a crear los objetos a partir de clases.

Por lo tanto, la forma que tiene Pharo (y muchos otros lenguajes orientados a objetos) es organizarse alrededor clases.

System Browser, el centro de nuestro desarrollo
-----------------------------------------------

Para comenzar vamos a necesitar una imagen de Pharo, cualquier imagen nos va a servir; pueden seguir usando la que tiene instalada Ozono :D.

Nuestro principal amigo en el desarrollo con Pharo va a ser el *System Browser*. Este lo encontramos dentro del menú principal de Pharo.

![](PharoParaOzonosos-World.png "PharoParaOzonosos-World.png")

Esto nos va a abrir la siguiente ventana (es bastante compleja para decirle ventanita).

![](PharoParaOzonosos-SystemBrowser.png "PharoParaOzonosos-SystemBrowser.png")

### Secciones del System Browser

Esta ventana se divide en distintas secciones, esta ventana es central, presenta mucha mucha mucha información por eso vamos a analizarla por secciones.

![](PharoParaOzonosos-SystemBrowserSections.png "PharoParaOzonosos-SystemBrowserSections.png")

Estas Secciones son:

-   **Categorias de clases:** en esta sección aparece una forma de categorizar las clases. Las categorías de las clases en Pharo

se llaman Packages, estos paquetes de clases pueden contener una o más subcategorías dentro.

-   **Clases:** una vez que selecciono un paquete se me van a mostrar las clases que están definidas en ese paquete. Porque algunas

aparecen con signo de admiración (!), porque no tienen escrita la documentación que explica que hace la clase.

-   **Categorías de Métodos:** cuando selecciono una clase se me van a mostrar los métodos que tiene definida esa clase. Estos

métodos pueden estar clasificados en distintas categorías, esto es muy útil como forma de documentación y suma mucho valor cuando comienza a crecer mi programa.

-   **Metodos:** al ir navegando las categorías de los métodos van a ir mostrándome la lista de métodos de esa clase en la categoría

seleccionada.

-   **Área de Edición de Código:** En esta área de la ventana es donde se realiza la edición del código. Si elegimos un método vamos a

ver el código de ese método, pero si elegimos una clase nos muestra un código raro. No entremos en pánico, ese código es la definición de la clase, ya vamos a entrar en el detalle de como hacerlo.

Por ahora debemos entender las distintas secciones del *System Browser* pronto vamos a ver para que se usa cada cosa con un ejemplo.

Workspaces
----------

Bueno, bueno, todo muy bonito pero a mi me gustaba enviar mensajes a mis objetos. Esto lo perdimos, esto es una mierda. No, no perdimos nada!!!

La forma de enviar mensajes y probar nuestro programa van a ser nuestros bonitos workspace (después vamos a hablar de tests, pero eso es otra versión mucho más cheta aunque menos interactiva).

![](PharoParaOzonosos-Workspace.png "PharoParaOzonosos-Workspace.png")

Para crear un workspace lo hacemos desde el menú principal de Pharo, es el mismo que usamos inicialmente.

Además podemos tener muchos workspace abiertos.

Los workspace de Pharo se usan de la misma manera que los de Ozono que ya usamos previamente. Pero con una variación interesante, podemos definir variables dentro del workspace. Estas variables son locales al workspace. Para definirlas debemos usarlas directamente, asignandole un valor. Cuando evaluemos esa línea va a crearse la variable.

Atención, los workspace no se van a guardar solitos como en Ozono, tenemos que guardarlos a mano. Normalmmente no nos vamos a preocupar por esto ya que el código que pongamos en el workspace es para pruebas rápidas. Si queremos guardar algo que vamos a probar varias veces nos sirve la idea de Tests, ya la vamos a ver; pero por ahora con el workspace nos sirve.

Los workspace se guardan como archivos de texto con extensión *.ws*, para poder guardar y cargar estos archivos tenemos que usar la flechita que esta a la derecha y arriba del workspace.

![](PharoParaOzonosos-Workspace-Menu.png "PharoParaOzonosos-Workspace-Menu.png")

Un ejemplo Paso a Paso
----------------------
