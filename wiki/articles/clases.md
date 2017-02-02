---
layout: article
title: Clases
---

### Motivación y Problema

¿Por qué necesito clases? Porque tengo varios objetos *sospechosamente parecidos*. Es decir,

-   El **comportamiento** de varios objetos es **igual**. Es decir,

  
-&gt; no solo entienden los mismos mensajes, sino que

-&gt; tienen los mismos métodos (exactamente el mismo código).

-   sus **atributos** son los **mismos**.
-   el **estado interno** es **diferente**.

  
Es decir, si bien tienen los mismos atributos, éstos apuntan a diferentes objetos.

-   su **identidad** es **diferente**.

  
Es decir, no sólo les damos diferentes nombres en diferentes referencias, sino que además son objetos diferentes.

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

-   Todo objeto es siempre instancia de una y sólo una clase.
-   No se puede cambiar la clase de un objeto una vez creado.

### Method Lookup

Si el código está en la clase, entonces, ¿Cómo responde ahora un objeto un mensaje? Con el Method Lookup. El method lookup es el mecanismo por el cual un objeto va a buscar el método correspondiente a su clase. (si no lo encuentra en su clase, no lo entiende). Revisar [Herencia](herencia.html) para conocer el Method Lookup completo.

### ¡Alto! ¿Quién soy yo?

Si mi código ahora no está en mi objeto, sino en una clase, **¿¿Quién es self??**

**`self`**` apunta siempre al `**`objeto` `receptor` `del` `mensaje`**`. `

En éste envío de mensaje:

`simba.rugi()`

Si el método rugi() de la clase Leon usa self internamente, simba es self.

### ¿Cómo construyo ahora mis objetos?

Mandándole el mensaje new a la clase correspondiente.

**`Smalltalk`**
`simba := Leon new.`
`mufasa := Leon new.`
`simba tuPapaEs: mufasa.`

**`Wollok`**
`var simba = new Leon()`
`var mufasa = new Leon()`
`simba.tuPapaEs(mufasa)`

Es importante entender que en las líneas del estilo

`  unaVar := UnaClase new.`

o

`  var unaVar = new UnaClase()`

pasan **dos** cosas, en el orden que se indica

1.  se crea un objeto instancia de la clase UnaClase.
2.  se hace que la variable unaVar haga referencia al objeto recién creado.

Puede perfectamente instanciarse una clase y no asignar una variable en la misma línea con el nuevo objeto, siempre depende de lo que se esté tratando de hacer.

### Diagramando Clases

Ver [Diagrama de clases](diagrama-de-clases.html)

### Conclusión

Una clase sirve de **fábrica** de objetos. Modela las **abstracciones** de mi dominio (los *conceptos* que nos interesan), permitiéndome definir el **comportamiento** y los **atributos** de las instancias.

Dependiendo del lenguaje las clases pueden ser un objeto más que entiende mensajes pensados para ellas (este es el caso de Smalltalk, ver [Variables y métodos de clase](variables-y-metodos-de-clase.html)) o una construcción distinta del lenguaje que sólo existe para declarar el comportamiento de los objetos y darnos una forma de obtener nuevas instancias mediante el uso de [constructores](constructores.html) (no envíos de mensajes) como sucede en Wollok.
