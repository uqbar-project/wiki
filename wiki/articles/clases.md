---
layout: article
title: Clases
---

### Motivación y Problema

¿Por qué necesito clases? Porque tengo varios objetos *sospechosamente parecidos*. Es decir,

-   El **comportamiento** de varios objetos es **igual**. Es decir, no solo entienden los mismos mensajes, sino que tienen los mismos métodos (exactamente el mismo código).
-   sus **atributos** son los **mismos**, pero el **estado interno** es **diferente**. Es decir, si bien tienen los mismos atributos, éstos pueden apunta a diferentes objetos.
-   su **identidad** es **diferente**. Es decir, incluso si se comportan igual y se encuentran en el mismo estado, no son el mismo objeto.

En conclusión, necesitamos una **abstracción** donde pongamos el código y los atributos en común de todos éstos objetos. Ésta abstracción es la **clase**, y cada objeto que se comporte igual (aunque tenga diferente estado interno) va a ser una **instancia** de esa clase.

Podemos pensar a las clases como "Especies" y a las instancias como "individuos" de esas especies

| Especie (**Clase**) | Individuo (**instancia**) |
|---------------------|---------------------------|
| Leon                | simba                     |
|                     | nala                      |
|                     | mufasa                    |
| Hiena               | shenzi                    |
|                     | banzai                    |
|                     | ed                        |
| Jabalí              | pumba                     |
| Zuricata            | timon                     |

### Cosas a recordar

- Todo objeto es siempre instancia de una y sólo una clase.
- No se puede cambiar la clase de un objeto una vez creado.
- Los métodos y atributos declarados en una clase son para sus instancias.

### Method Lookup

Si el código está en la clase, entonces, ¿Cómo responde ahora un objeto a un mensaje? Con el Method Lookup. El method lookup es el mecanismo por el cual un objeto va a buscar el método correspondiente a su clase. (si no lo encuentra en su clase, no lo entiende). Revisar [Herencia](herencia.html) para conocer el Method Lookup completo.

### ¡Alto! ¿Quién soy yo?

Si mi código ahora no está en mi objeto, sino en una clase, **¿¿Quién es self??**

**`self`** apunta siempre al **objeto receptor del mensaje**. 

En éste envío de mensaje:

`simba.rugi()`

Si el método rugi() de la clase Leon usa self internamente, simba es self.

### ¿De dónde salen las instancias?

Si definimos una clase, por ejemplo Leon, y queremos obtener a simba que es una instancia de Leon para poder mandarle mensajes, necesitamos crear la instancia a partir de la clase Leon. O sea que la clase tiene un segundo rol importante, no sirve sólo para definir el comportamiento y los atributos, también sirve para crear los objetos que luego usaremos en nuestro programa.

Dependiendo del lenguaje, esto puede hacerse de formas distintas.

En lenguajes en los cuales las clases son objetos, como es el caso de Smalltalk o Ruby por ejemplo, esto se hace mandándole un mensaje a la clase correspondiente. Por ejemplo:

```Smalltalk
simba := Leon new.
mufasa := Leon new.
simba tuPapaEs: mufasa.
```

En otros lenguajes, como en Wollok o Java, las clases no son objetos y para crear instancias se utilizan [herramientas específicas para la construcción de objetos](herramientas-de-instanciacion.html). Por ejemplo:

```Wollok
var simba = new Leon()
var mufasa = new Leon()
simba.tuPapaEs(mufasa)
```

Es importante entender que en las líneas del estilo `unaVar := UnaClase new.` o `var unaVar = new UnaClase()` pasan **dos** cosas, en el orden que se indica:

1.  se crea un objeto instancia de la clase UnaClase.
2.  se hace que la variable unaVar haga referencia al objeto recién creado.

Puede perfectamente instanciarse una clase y no asignar una variable en la misma línea con el nuevo objeto, siempre depende de lo que se esté tratando de hacer. Por ejemplo podría crearse un objeto dentro de un método y retornarlo directamente, o crearlo para mandarle un mensaje directamente.

Si una instancia no es referenciada desde ningún lado, la misma ya no podrá ser usada, ya que nadie va a poder mandarle mensajes. Sin embargo no debemos preocuparnos por la memoria que ocupen estos objetos no olvidados, ya que ese trabajo es del [Garbage Collector](garbage-collector.html).

### Conclusión

Una clase sirve de **fábrica** de objetos. Modela las **abstracciones** de mi dominio (los *conceptos* que nos interesan), permitiéndome definir el **comportamiento** y los **atributos** de las instancias.

Dependiendo del lenguaje las clases pueden ser un objeto más que entiende mensajes pensados para ellas (este es el caso de Smalltalk, ver [Variables y métodos de clase](variables-y-metodos-de-clase.html)) o una construcción distinta del lenguaje que sólo existe para declarar el comportamiento de los objetos y darnos una forma de obtener nuevas instancias mediante el uso de [herramientas de instanciación](herramientas-de-instanciacion.html) (no envíos de mensajes) como sucede en Wollok.

Lo importante de todo esto es que las clases no son construcciones centrales como sí son los objetos, ya que impactan sólo a la definición y creación de los mismo, pero para el uso general del sistema, trabajamos de la misma forma que si las clases no estuvieran ahí. Es por eso que es en estos puntos en donde más difieren los lenguajes existentes, sin embargo la idea de objeto - mensaje se mantiene.
