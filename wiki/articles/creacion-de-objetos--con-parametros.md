---
layout: article
title: Creacion de objetos  con parametros
---

Eso, lo que dijo Leo. Pero algunas cositas más ...

En Smalltalk no hay constructores, hay mensajes y ya.

Todos los mensajes devuelven un objeto (p.ej. cuando le ponés el ^ sombrerito). Los métodos que no tienen el sombrerito, es como si abajo de todo dijeran ^self.

p.ej. Golondrina &gt;&gt; energia: unaEnergia

`        "Setter de la variable energia"`
`        energia := unaEnergia.`
`        ^self`

Entonces no hay constructores, pero podemos hablar de mensajes que le mandamos a la clase (mensajes de clase) y que devuelven una instancia de esa clase. Lo que podés hacer es implementar tus propios métodos de clase.

El mensaje new no recibe parámetros, así que con new no vas a poder hacer eso que decís

p.ej.

<En un workspace> pepita := Golondrina nuevaEnergiaInicial: 100 ubicacionActual: buenosAires. &lt;\\En un workspace&gt;

El objeto que recibe el mensaje es la clase Golondrina, entonces self dentro del método \#nuevaEnergiaInicial:ubicacionActual: apunta a la clase Golondrina

------------------------------------------------------------------------

(Método de clase de Golondrina) nuevaEnergiaInicial: unaEnergia ubicacionActual: unLugar

`    | nuevaGolondrina |`

`    "Instanciamos una nueva Golondrina, acordate que self apunta a la clase Golondrina en este ejemplo"`
`    nuevaGolondrina := self new.`

`    "Le mandamos el mensaje energia: a la nueva instancia"`
`    nuevaGolondrina energia: unaEnergia.`

`    "Le mandamos el mensaje ubicacion: a la nueva instancia"`
`    nuevaGolondrina ubicacion: unLugar.`

`    "Si no escribimos nada más devolvería self, en este caso la clase Golondrina, y se rompería todo en el workspace cuando le empecemos a hablar a pepita (que apuntaría a la clase Golondrina en vez de a una nueva instancia"`

`    "Por todo eso agregamos"`
`    ^nuevaGolondrina`

------------------------------------------------------------------------

De todas formas en el propio workspace se puede instanciar en menos líneas sabiendo que los mensajes que son setters devuelven el objeto receptor de ese mensaje

"Puse los paréntesis en rojo para que se entienda más, pero no son necesarios"

pepita := ((Golondrina new) energia: 100) ubicacion: buenosAires.

Como es peligroso asumir eso y no ayuda a la expresividad, se puede usar un operador punto y coma &lt;;&gt; (que es un chiche sintáctico de Smalltalk) para mandar mensajes en cascada.

Object &gt;&gt; yourself

`         "El mensaje yourself devuelve self, cuac!"`
`         ^self`

Escribir pepita := Golondrina new.

Es igual a escribir pepita := (Golodrina new) yourself.

Escribir "Evaluar todo esto nos devuelve una instancia de la clase Golondrina, con 100 de energia y con buenosAires como ubicación" pepita := Golondrina new. pepita energia: 100. pepita ubicacion: buenosAires. pepita.

Es lo mismo que pepita :=

`       Golondrina new`
`               energia: 100;`
`               ubicacion: buenosAires;`
`               yourself`

Bue, me pareció copado contar todo esto :$

Si no se entendió algo pruébenlo, usen el inspect y tiene que salir. O de última, vuelvan a preguntar =P

Saludos,

El 9 de mayo de 2009 14:16, Leonardo "Pelado" Cesario &lt;peladosnow@gmail.com&gt; escribió:

`   Hernan:`
`   Primero te preguntaria si ya viste metodos de clase.`
`   En caso afirmativo vos podes hacer tu metodo de creacion, por ejemplo new:`
`   No es verdad que el new envia el mensaje initialize (por lo menos no pasa en todas las versiones de ST)`
`   Si podes redefinir el new para que llame al initialize. De todas formas la idea del initialize es inicializar (cuak!) el estado interno, no de pasarle parametros.`
`   El tema de si lo creo con parametros o luego se los mando por medio de otro mensaje, es un tema de diseño.`
`   Espero se aclare la duda y te ayude a decidir.`
`   Saludos!`
`   Leo C`
