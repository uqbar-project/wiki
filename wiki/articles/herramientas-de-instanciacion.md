---
layout: article
title: Herramientas de instanciación
---

Llamamos instanciación a la creación de un objeto a partir de una [clase](clases.html), la cual define cuál es el comportamiento y los atributos que debería tener dicha instancia.

Los mecanismos para instanciar objetos a partir de una clase varían bastante entre los distintos lenguajes. En particular, en aquellos lenguajes en los cuales las clases no son a su vez objetos (como Wollok y Java por porner algunos ejemplos) se proveen herramientas para instanciar objetos que se escapan un poquito de la mecánica más común de trabajo que es la de enviarle un mensaje a un objeto.

En general, desde el punto de vista del uso, lo que los lenguajes proveen es una forma de crear cada instancia indicando cuál es la clase que se está instanciando y, en caso de que sea necesario, con qué valores debe inicializarse la instancia en cuestión. De esa forma, el objeto ya estará listo para su uso desde el momento de la creación.

A continuación se detallan algunas herramientas de construcción a modo de referencia.

## Inicialización directa

Este es el mecanismo disponible en el lenguaje Wollok en su versión actual. Permite crear instancias de una clase parametrizando los valores iniciales para los atributos que correspondan usando **parámetros nombrados**. Supongamos que tenemos una clase Golondrina con un atributo energia, cuyo valor por defecto es 100:

```Wollok
class Golondrina {
  var energia = 100
  // ... todos los otros métodos para las golondrinas, 
  // entre los cuales habrán muchos que trabajen con la energía asumiendo que está inicializada con algún valor numérico
}
```

Podemos crear una instancia de la clase Golondrina manteniendo el default para la energía de esta forma: `new Golondrina()`. Si nos interesara crear una golondrina con 25 de energía, la inicialización directa permite parametrizar esta información de esta forma: `new Golondrina(energia = 25)`, y el default simplemente no se usará.

En caso de haber varios atributos, pueden inicializarse tantos como querramos, por ejemplo: `new Direccion(calle = "Amenabar", numeracion = 140)`. No importa el orden en el cual se indiquen estos parámetros, solamente debe coincidir con el nombre del atributo que se está inicializando.

Esta herramienta, combinada con buenos defaults en caso de que existan, permite construir objetos listos para usar de forma sencilla y con muy poca burocracia.

## Constructores

> **Nota:** Los ejemplos que se muestran a continuación son en el lenguaje Wollok en sus versiones previas a la 1.8, sólo a modo ilustrativo.

Los **constructores** son métodos especiales cuya finalidad es la creación de objetos de la clase indicada con la inicialización de estado interno correspondiente. Para invocarlos se usa la palabra reservada **new** seguido por el nombre de la clase y los parámetros según corresponda. Este es el mecanismo que se puede encontrar en varios lenguajes industriales muy usados, como Java y C#, y también fue el mecanismo soportado por el lenguaje Wollok en sus primeras versiones (luego se deprecó intrudiciendo el mecanismo de **inicialización directa**). 

En general esta forma de definir lógica de inicialización permite definir más de un constructor con distinta cantidad de parámetros, de modo que se permita al usuario elegir la forma más adecuada para crear las instancias. Por ejemplo, las fechas podrían permitir instanciarse con un constructor sin parámetros retornando la fecha del día de hoy (porque es muy común querer obtener instancias con esa configuración particular, entonces se considera como la inicialización por defecto si no se indica nada), o con otro de 3 parámetros para que al instanciar la fecha se la inicialice con el día, el mes y el año deseados:

```Wollok
new Date() // retornaría la fecha de hoy
new Date(22,10,2016) // retornaría la fecha para el día, mes y año especificados.
```

Esto permite que siempre que obtengamos un objeto, el mismo ya esté en condiciones de ser usado en vez de requerir que el usuario recuerde indicar los valores necesarios uno por uno mediante setters, teniendo mientras tanto un objeto con un estado inválido que no puede ser usado.

Si no definimos ningún constructor, por defecto la clase viene con un constructor vacío implícito para poder crear los objetos haciendo, por ejemplo: `new Golondrina()`

Supongamos que queremos construir golondrinas con una cantidad de energía que nosotros elijamos como valor inicial, podríamos definir:

```Wollok
class Golondrina {
  var energia
  
  constructor(_energia){
    energia = _energia
  }
  // ... todos los otros métodos para las golondrinas, 
  // entre los cuales habrán muchos que trabajen con la energía asumiendo que está inicializada con algún valor numérico
}
```

Al definir ese constructor, el constructor vacío por defecto ya no estará disponible para instanciar objetos, de esa forma sólo podremos crear objetos con un estado interno válido en base al constructor particular que hayamos definido.

Si el lenguaje que usás usa constructores para definir cómo se inicializan las instancias, asegurate de investigar no sólo cuestiones sintácticas, sino también qué recaudos hay que tener respecto a la [herencia](herencia.html).
