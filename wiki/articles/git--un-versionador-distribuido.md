Distribuido...
--------------

Se dice de git que es un sistema de control de versiones distribuido. Que sea distribuido implica que van a existir al menos tantos repositorios como personas trabajando en el proyecto, a lo que se le pueden sumar uno o más repositorios que se usen como intermediarios para compartir cambios entre las distintas personas. Un lugar práctico para hostear repositorios públicos puede ser \[<https://github.com/>| gtihub\]

Repositorio
-----------

Por repositorio se entiende el lugar donde se van guardando todas las versiones de un proyecto o de cada una de las cosas que forman parte de él.

En el mundo git en vez de pensar en que cada archivo es una parte que va cambiando de versiones prefirieron pensar en que es todo el proyecto el que se va versionando entonces cada vez que se modifica una parte en realidad se está modificando todo el proyecto.

Un proyecto no es otra cosa que una arbol de directorios y archivos donde la raíz del arbol es la carpeta raíz del proyecto. Este Arbol es lo que va cambiando a medida que avanza el proyecto.

La forma en que git elige modelar el proyecto es como un arbol por lo que en el interior de un repositorio git van a existir tantos árboles como versiones distintas existan del proyecto. Estos árboles van a formar parte de un ¿DAG?

DAG ¿lo qué?
------------

Veamos primero un dibujito:

![](Figura1.jpg "Figura1.jpg")

DAG es la forma cheta de decir grafo dirigido acíclico. O sea un dibujo con circulitos (nodos) y flechitas (vértices) que unen los circulitos sin que se pueda llegar al mismo círculo saltando de círculo en círculo siguiendo las flechitas que los unen.

Un árbol es de por sí un DAG por lo que no tiene problemas en ser parte de un DAG más grande que lo contiene.

Cada uno de los círculos representa un objeto almacenado en el repositorio (nodos en el DAG). Los rectángulos son referencias que sirven como punto de entrada a los objetos que viven en el repositorio, podrían considerarse parte del DAG pero están representados como rectángulos para diferenciar las referencias de los objetos almacenados.

Los distintos colores de los circulos representan cada uno de los 4 tipos de objetos que existen:

-   blob. Representa un archivo en el proyecto.
-   tree. Representa una carpeta en el proyecto, el primero corresponde al directorio raiz del proyecto o el mismísimo proyecto.
-   commit. La forma de agregar objetos al repositorio es con la operción commit. Esto agrega un objeto commit que tiene un vértice hacia un tree que representa al directorio raíz del proyecto. El sub-grafo que comienza en este tree representa el estado de todo el proyecto tras la ejecución del commit. En el dibujo se ve que los subgrafos que parten de cada commit comparten un objeto blob, esto significa que el commit B no modificó el archivo que agregó el commit A.

El commit también conoce a su padre que es el commit que lo antecede, entonces los cambios que aplica un commit al proyecto son las diferencias entre el árbol al que apunta y el árbol al que apunta su padre. Cuando se ejecuta la operación merge se qenera un commit que tiene al menos dos padres entonces los cambios que aporta este commit depende de contra cual de sus padres se lo compare.

-   tag. La operación tag genera un objeto tag en el repositorio y una referencia de tipo tag para accederlo.

Un tag conoce un commit y a partir de este el estado del proyecto para ese commit. En su cuerpo también uncluye un título un comentario y opcionalmente se le puede agregar una firma gpg. Todos los objetos (círculos en el dibujo) tiene un nombre bastante feo para identificarlos (algo así como 8357799df0b47164c9726be6610ea1b7ed41ff32), este nombre es el resultado de aplicar un función de hash (SHA1) al objeto.

Referencias: Las referencias son los rectángulos en el dibujo, son simplemente puntos de entrada a los objetos almacenados en el repositorio.

-   Referencia de tipo tag (amarillo en el dibujo) apunta a un objeto de tipo tag y es inmutable, o más o menos inmutable.
-   Referencia de tipo branch (verde en el dibujo) apunta a un ojeto commit, es mutable y al commit al que apunta es al \* último que se agregó para ese branch.
-   Referencia de tipo branch remoto (gris en el dibujo) es muy parecida a un branch sólo que se usa para seguir el estado de un branch en otro repositorio (recuerden que esto era distribuido). La del dibujo (origin/master) significa que sigue un branch llamdo master en un repositorio llamado origin. El nombre del repositorio es un shortcut de la url completa.
-   HEAD esta referencia la usa git para saber que commit será el padre del próximo commit, normalemnte apunta indirectamente a un commit a travez de un branch. Si almomento de ejecutar la operción commit el HEAD apunta a un branch el branch se actualiza para apuntar al nuevo commit.

Todo muy lindo... ¿pero cómo lo uso?
------------------------------------

Bueno... primero lo tendrías que tener instaldo. Eso se explica \[\[instalando\_git\]| acá\]. Ahora que git ya está git intalado y configurado podemos ver un ejemplo partiendo de un proyecto nuevo.

En la consola (git bash si estás en windows)

`$ mkdir ~/prueba`

$ cd ~/prueba $ git init

Lo que estamos haciendo es comenzar un proyecto cuya carpeta raíz será ~/prueba, y en esa carpeta inicializamos un repositorio git para mantener las versiones de nuestro proyecto de prueba. El repositorio está en una carpeta llamada .git adentro de la carpeta raíz del proyecto. La carpeta ~/prueba con todo su contenido menos la carpeta .git es lo que git llama working tree, algo similar a la working copy de svn.

Git lo llama working tree porque un directorio en el file system que estemos usando es también un árbol y como git entiende los proyectos como árboles dice que la carpeta donde editamos los archivos del proyecto no es otra cosa que el árbol de trabajo.

El repositorio no tiene todavía ningún objeto almacenado (círculos del dibujo) pero sí, tiene la referencia HEAD que apunta a ref/heads/master esto siginifica que apunta a un branch llamado master, el nombre master es el que se les ocurrio a los amigos de git para ponerle al branch principal, algo así como el trunk del svn. Un branch no es otra cosa que una referencia a un commit, pero como todavía no existe ningún commit tampoco existe el branch master. La referencia HEAD sirve para que git sepa en que branch estamos trabajando actualmente, lo que significa que al hacer el próximo commit actualizará el branch apuntado por HEAD. En este caso que todavía no se hizo ningún commit el branch master se creará con el primer commit.

Vamos a crear nuestro primer commit

`$ cd ~/prueba #carpeta raíz del proyecto`

$ touch unTexto.txt \#creo un archivo $ git add unTexto.txt \#agrego el archivo al index de git $ git commit -m "primer commit" \#ejecuto la operación commit pasándole el comentario "primer commit"

Basicamente la forma de trabajo es:

Modifico el contenido del working tree (agrego y modifico archivos) Agrego alguno o todos los cambios al index (git add) Ejecuto un commit (git commit) Los cambios que aporta el commit es todo los que esté en el index. ¿Y qué es el index? Es un lugar donde voy acomodando los cambios que van a entrar en el próximo commit. También llamado staging, pero en casi toda la documentación de git aparece como index.

como quedó el grafo de nuestro repo:
