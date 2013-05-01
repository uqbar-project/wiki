### Motivación y Problema

¿Por qué necesito clases? Porque tengo varios objetos *sospechosamente parecidos*. Es decir,

-   El **comportamiento** de varios objetos es **igual**. Es decir,

  
-&gt; entienden los mismos mensajes

-&gt; Y tienen los mismos métodos (exactamente el mismo código).

-   sus **atributos** son los **mismos**.
-   el **estado interno** es **diferente**.

  
Es decir, si bien tienen los mismos atributos, éstos apuntan a diferentes objetos.

-   su **identidad** es **diferente**.

  
Es decir, no sólo les damos diferentes nombres en diferentes referencias, sino que además son objetos diferentes.

Conclusión, necesitamos una **abstracción** donde pongamos el código y los atributos en común de todos éstos objetos. Ésta abstracción es la **clase**, y cada objeto que se comporte igual (aunque tenga diferente estado interno) va a ser una **instancia** de esa clase.

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

Si el código está en la clase, entonces, ¿Cómo responde ahora un objeto un mensaje? Con el Method Lookup. El method lookup es el mecanismo por el cual un objeto va a buscar el método correspondiente a su clase. (si no lo encuentra en su clase, no lo entiende). Revisar \[Herencia\] para saber

### ¡Alto! ¿Quién soy yo?

Si mi código ahora no está en mi objeto, sino en una clase, **¿¿Quién es self??**

**`self`**` apunta siempre al `**`objeto` `receptor` `del` `mensaje`**`. `

En éste envío de mensaje:

`simba rugi`

simba es self. En éste otro:

`Leon new`

La clase Leon es self.

### ¿Cómo construyo ahora mis objetos?

Mandándole el mensaje new a la clase correspondiente.

`simba := Leon new.`
`mufasa := Leon new.`
`simba tuPapaEs: mufasa.`

### Conclusión

Una clase es un objeto cuya responsabilidad es servir de **fábrica** de objetos. Modela las **abstracciones** de mi dominio, permitiéndome definir el **comportamiento** y los **atributos** de las instancias.
