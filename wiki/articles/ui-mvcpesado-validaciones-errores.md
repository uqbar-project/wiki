---
layout: article
title: Validaciones y manejo de errores en la UI
categories: [arena, validaciones, errores]
featured: true
---

Podemos ver un repaso del tema excepciones en [esta página](excepciones-avanzadas.html)

# Introducción

Consideremos el ejemplo de los clientes de una empresa de celulares, donde tenemos un formulario que permite ingresar

- nombre del cliente
- número de celular
- modelo de celular
- si quiere recibir el resumen de cuenta en el domicilio

# Validaciones a implementar

1. El número debe contener sólo dígitos numéricos
1. El modelo de celular debe ser un modelo válido
1. Los números de celular deben ser mayores a 1000
1. No puede ingresarse el mismo número de teléfono para dos clientes diferentes
1. Algunos modelos de celular exigen que sus clientes reciban el resumen de cuenta en su domicilio

¿Qué hacemos en cada caso? ¿Quién es responsable de cada validación?

# Momentos de la validación

## El número debe contener sólo dígitos numéricos

Si el objeto de dominio Celular define el número de teléfono como un Integer, no es posible hacer

```scala
celular.numero = "A"
```

eso **no compila**. Pero la UI podría tener un cuadro de texto que permita ingresar caracteres alfanuméricos: entonces tenemos que elegir cuál va a ser el comportamiento del sistema

1. permitir ingresar caracteres inválidos pero mostrar un mensaje de error: en Arena esto lo hace por defecto el controller que adapta lo que el usuario carga a lo que el dominio necesita. El panel de errores (ErrorsPanel) captura cualquier excepción que ocurra en la conversión, ya sea que lo incluyamos manualmente o por una ventana que herede de `SimpleWindow`.
1. podríamos pensar: ¿para qué dejamos que el usuario ingrese un caracter inválido si luego lo vamos a rechazar? La segunda variante consiste en definir un filtro que no permita que el usuario pueda ingresar caracteres alfabéticos si queremos que ingrese números. Para esto...

### Opción 1: Definimos un filter

Esta es la versión en lenguaje Java:

```java
new TextBox(form)
    .withFilter(new TextFilter {
        public boolean accept(TextInputEvent event) {
            return StringUtils.isNumeric(event.getPotentialTextResult());
        }
    })
    .bindValueToProperty(Celular.NUMERO);
```

La misma versión en lenguaje Xtend:

```scala
new TextBox(form) => [
    withFilter [ event | event.potentialTextResult.matches("[0-9,.]*") ]
    bindValueToProperty("numero")
    width = 100
]
```

¿Qué es el TextFilter dentro del MVC? El TextFilter es un **controller**, porque se comunica con el dominio _(le manda un mensaje al modelo - en el caso de estar ok el input)_ y actúa sobre la vista _(en la pantalla puede no aparecer ese caracter)_.

### Opción 2: Utilizamos un control específico para ingresar números

Desde Arena 3.6.1 tenemos un control NumericField que se encarga de filtrar los caracteres alfabéticos:

```java
// Java
new NumericField(form).setWidth(150).bindValueToProperty("numero");
```

```scala
// Xtend
new NumericField(form) => [
    value <=> "numero"
    width = 100
]
```

Internamente está utilizando un Filter, lo importante es que tenemos una abstracción de alto nivel que está diciendo "quiero un control donde sólo se puedan cargar números".

Entonces la primera variante es impedir cualquier ingreso inválido por parte del usuario: Esto tiene como ventaja ser fail fast, evita ingresos incorrectos y esto para el usuario es más beneficioso.

## Otras formas de evitar acciones incorrectas

Siguiendo la anterior linea de pensamiento, ¿debería dejar que el usuario presione el botón Aceptar, solo para mostarle un mensaje de error después? ¿no debería habilitar el botón Aceptar solamente cuando todos los controles se hayan cumplido satisfactoriamente?

Aquí vemos que esta estrategia tiene un límite: en ciertos casos las validaciones del negocio pueden ser verdaderamente complejas como para poder dejarlas en forma explícita en la pantalla. Entonces el usuario sentirá una lógica frustración de no poder avanzar con el caso de uso cuando el botón Aceptar esté inhabilitado y no quede claro por qué. **Una regla importante para la usabilidad de un sistema es que debe explicar claramente al usuario qué información no cumple las reglas de negocio y además cómo debe continuar para llegar al caso exitoso**.

# Implementando las validaciones restantes

## El modelo de celular debe ser un modelo válido

¿El responsable es el celular? sí, si lo pensamos como un dato obligatorio, pero claramente participan

1. la UI que guía al usuario mostrándole un combo con las opciones válidas, el usuario no puede elegir un modelo inexistente, a lo sumo puede dejarlo vacío
1. si yo permito que el combo quede vacío, el dominio (el objeto celular) debería validar que el celular no deje en blanco el campo modelo. El form builder permite en sus opciones decirle "este combo no tiene la opción vacía".

```java
// Java
Selector<ModeloCelular> selector = new Selector<ModeloCelular>(form) //
     .allowNull(false);
```

```scala
// Xtend
new Selector<Modelo>(form) => [
     allowNull(false)
```

El tema es que al dar de alta un celular el binding es contra un atributo nulo, entonces el combo queda igualmente vacío. ¿Dónde voy a poner la validación del celular? En la clase Celular, un método validar() ¿qué va a devolver? void, o exception si hubo error... nada de códigos de error numéricos, como saben. Porque si todo sale bien sólo sigo enviando mensajes a los objetos que corresponden. Y si algo sale mal se que tengo que atrapar una excepción en la vista.

Entonces no acoplo innecesariamente las validaciones del modelo a la vista. La pantalla de edición sólo tiene que saber que pueden ocurrir dos tipos de error posibles:

- **errores de negocio:** surgen de las restricciones que el negocio va poniendo (no podés ingresar un modelo inexistente, no podés poner caracteres alfabéticos en la línea del celular). Esto lo modelamos con una UserException.
- **errores de sistema:** son errores propios de la programación, o errores generales del sistema (algo se rompió). Estos no los modelamos, simplemente ocurren: son los NullPointerException, OutOfMemoryError, etc.
Codificamos entonces el método validar:

```java
// Java
public void validar() {
   ...
   if (this.modeloCelular == null) {
      throw new UserException("Debe ingresar un modelo de celular");
   }
}
```

```scala
// Xtend
def void validar() {
    ...
    if (modeloCelular == null) {
        throw new UserException("Debe ingresar un modelo de celular")
    }
}
```

¿Quién debe atrapar esta excepción que tira el negocio? 

Eso tiene que estar del lado de la tecnología de presentación: en la vista o más precisamente en el controller, allí debe estar el bloque try/catch para trabajar tanto los errores de negocio como los de sistema:

- si ocurre un error de usuario/negocio, el mensaje contiene información importante para el usuario. Entonces hay que mostrarle un cartel (o dejar una parte específica del panel para mostrar errores) con lo que contenga la propiedad message de la excepción de usuario/negocio
- por el contrario, si el error se da dentro del programa, mostrar el mensaje de error al usuario le genera confusión: lo mejor que uno puede hacer es advertirle al usuario que hubo un error, que la operación que solicitó no va a poder completarse y sobre todo, registrar el problema para que un desarrollador lo analice luego.

En [este artículo](http://www.google.com/url?q=http%3A%2F%2Fwiki.uqbar.org%2Fwiki%2Farticles%2Fvalidaciones-y-manejo-de-errores-en-la-ui.html&sa=D&sntz=1&usg=AFQjCNEloVJhOBmEdc5DLJxg8nutWCj7uw) podés profundizar algunas cuestiones.

## Los números de celular deben ser mayores a 1000

Si ponemos la validación en el setter

```java
// Java
public void setNumero(Integer numero) {
    if (numero < MAX_NUMERO) {
        throw new UserException("El número de celular debe ser mayor a " + MAX_NUMERO);
    }
    this.numero = numero;
}
```

```scala
// Xtend
def void setNumero(Integer unNumero) {
    if (unNumero != null && unNumero.intValue() <= MAX_NUMERO) {
        throw new UserException("El número debe ser mayor a " + MAX_NUMERO)
    }
    this.numero = unNumero
}
```

eso tiene como consecuencia que **tanto en la búsqueda como al editar un celular** yo tenga que poner un número de celular mayor a 1000. ¿Tiene sentido? Y... en parte sí porque si yo no puedo ingresar un celular mayor a 1000 no tiene sentido que pueda buscar un celular menor a 1000, no lo voy a encontrar (en una aplicación comercial me reportaron en un caso de prueba que no debería poder buscar por CUIT si ese CUIT no es válido).

Si ubicamos la pregunta en un método validar y lo llamamos en onClick del botón Aceptar...

```java
// Java
public void validar() {
    if (!this.ingresoNumero()) {
        throw new UserException("Debe ingresar número");
    }
    if (this.numero.intValue() <= MAX_NUMERO) {
        throw new UserException("El número debe ser mayor a " + MAX_NUMERO);
    }
    ...
```

```scala
// Xtend
def validar() {
    if (numero == null) {
        throw new UserException("Debe ingresar número")
    }
    if (numero.intValue() <= MAX_NUMERO) {
        throw new UserException("El número debe ser mayor a " + MAX_NUMERO)
    }
    ...
```

el efecto que tiene es que pude ingresar números menores a 1000 en la búsqueda pero no en la edición.

O sea, cuando yo tuve que poner esta validación, dudé entre ponerlo en:

1. el setter del atributo número
1. el método validar de celular, que se dispara al presionar el botón aceptar.

Ahora, validar que el nombre y el número no sean nulos claramente no está bueno incorporarlo en el setter porque entonces la búsqueda me fuerza a escribir algo tanto en el nombre como en el número. Nuevamente aparece una tensión de fuerzas: la filosofía fail fast me dice que debería tirar error tan pronto como sea posible, eso tiene como consecuencia que a medida que estoy ingresando una fecha o un rango de valores estoy recibiendo continuos mensajes de error que distraen mi foco de atención. Por otra parte esperar a que un formulario con muchos campos se complete para validar produce una sensación frustrante al querer aceptar: "Falta xxx", "Debe completar yyy", "La fecha es inválida", etc.

¿Cuál es la solución? No hay una única respuesta como se imaginarán,

- algunas validaciones irán en los filtros
- otras pueden ir en los setters
- otras deberán esperar su turno en la validación del formulario
- y la estrategia resultante será una combinación de todas ellas

## No puede repetirse el mismo número de celular para dos clientes

Esto no lo puedo validar en Celular, porque un objeto sólo tiene validaciones atómicas. Un celular no conoce a todos los otros celulares, me costaría mucho trabajo hacer que eso sucediera. Pero hay otro objeto que sí conoce a todos los celulares: el home/repositorio. 

Por suerte en la implementación del Repo default en Arena, el método create delega posibles validaciones en un "hook method" específico, que se llama validateCreate:

En `AbstractAutogeneratedIdRepo<T>` donde T es un Celular (hereda de `Entity`):

```java
public void create(T object) {
    this.validateCreate(object);
    ...
```

Definimos la validación en RepositorioCelulares:

```java
// Java
@Override 
public void validateCreate(Celular celular) {
    celular.validar();
    validarClientesDuplicados(celular);
}

public void validarClientesDuplicados(Celular celular) {
    int numero = celular.getNumero();
    if (!this.search(numero).isEmpty()) {
        throw new UserException("Ya existe un celular con el número: " + numero);
    }
}
```

```scala
// Xtend
override validateCreate(Celular celular) {
    celular.validar()
    validarClientesDuplicados(celular)
}
 
def void validarClientesDuplicados(Celular celular) {
    val numero = celular.numero
    if (this.search(numero).isEmpty) {
        throw new UserException("Ya existe otro cliente con el mismo número")
    }
}
```

¿Qué pasa en la modificación?

- Actualizar una colección en memoria es simplemente borrar el objeto anterior y agregar el objeto editado, eso está definido en CollectionBasedRepo.
- Arena no definió un método validateUpdate() para incorporar validaciones. Esta limitación suele ser común al utilizar frameworks, eso no evita que podamos interceptar el mensaje update, de la siguiente manera:

```scala
override update(Celular celular) {
    validarClientesDuplicados(celular)
    super.update(celular)
}
```

Al fin y al cabo estamos trabajando con objetos.

¿Y la validación cuándo se dispara? Cuando aceptamos el formulario, porque determinar si el número de celular se repite con otro tiene sentido cuando terminamos de definir el número, y además esa operación tiene un costo.

## Obligatoriedad de recibir resumen de cuenta en domicilio

Cada celular pertenece a un modelo de celular que define si debe recibir el resumen de cuenta en domicilio. ¿Cómo afecta eso a la ventana de edición?

- Una opción es que la vista no sufra modificaciones: en el método validar chequeamos que el usuario haya cargado una configuración válida para el modelo de celular y recepción de resumen de cuenta en domicilio.
- Pero para el usuario esta decisión puede resultar desconcertante: ¿qué modelo será el que me permita dejar destildado el check? Por eso para diseñar una buena UI necesitaríamos hacer algunas modificaciones: si el modelo de celular obliga a que el cliente reciba el resumen de cuenta, debería 
  - automáticamente ponerse en true el flag recibeResumenCuenta para el cliente
  - deshabilitarse la opción de modificar el check

Entonces vemos que la validación original en el celular pierde sentido: la interfaz va guiando al usuario impidiendo que tome decisiones incorrectas y minimizando así las ventanas de error según las restricciones que impone el negocio. Eso no implica sacar la validación, pero sí tener en mente la famosa experiencia de usuario para anticiparnos a ingresos incorrectos.

Aunque no siempre es posible evitar los mensajes de error, como hemos visto con la duplicidad de números de celular (no podemos saberlo sin consultar al repositorio), estas ideas de diseño en la UI mejoran notablemente su usabilidad.

# Algunas conclusiones

## Desventajas de las excepciones

Como las excepciones cortan el flujo normal de envío de mensajes entre objetos, la contra de tirar una excepción por cada error de negocio es que no nos permite decirle al usuario todos los campos que tienen problemas (los errores van apareciendo de a uno). Por eso otra técnica es "recolectar" los errores y asociarlos a campos, de manera de tener un listado de mensajes de error donde cada uno está asociado a un campo que se ingresa en el formulario.

## Pop-ups y paneles de error

Volviendo al ejemplo de los celulares, vemos que la validación tira User Exceptions:

```scala
def void validar() {
      if (!this.ingresoNumero) {
            throw new UserException("Debe ingresar número")
      }
    ...
}
```

Lo mismo ocurre con validaciones que hacen los repos:

```scala
private void validarClientesDuplicados(Celular celular) {
      val numero = celular.numero
      if (!this.search(numero).isEmpty) {
            throw new UserException("Ya existe un celular con el mismo número")
      }
}
```

Pero esa excepción no está atrapada en el método asociado al botón Aceptar de la pantalla de Edición.

```scala
new Button(actions)
      .setCaption("Aceptar")
      .onClick [ | this.accept]
      .setAsDefault
      .disableOnError
```

¿Cómo es entonces que funciona bien, que los errores se muestran con un popup?

- cada control de arena tiene un realidad abajo un builder que crea finalmente los objetos de la tecnología. En nuestro caso objetos swt + jface
- por ejemplo, al crear un SimpleWindow nosotros estamos utilizando (sin saberlo) un JFaceWindowBuilder que le construye el ErrorViewer (esto no ocurre si nuestra ventana hereda de Window solamente)
- Cuando se crea un botón, por ejemplo, y le seteamos un Action en el onClick, esta capa de arena le agrega un listener (un observer) que envuelve el command que nosotros construimos incorporándole el manejo de errores:

```java
@Override
public void widgetSelected(SelectionEvent event) {
      try {
            this.action.execute();
      } catch (UserException exception) {
            this.context.getErrorViewer().showWarning(exception.getMessage());
      } catch (RuntimeException exception) {
            exception.printStackTrace();
            this.context.getErrorViewer().showError("Se produjo un error de sistema. Puede revisar el log de la aplicación para obtener más detalles");
      }
}
```

El bloque catch puede variar dependiendo de la versión de Arena que estén usando. Pero más allá de algunos detalles de implementación que pueden ver ustedes, lo importante es ver qué sucede con los dos tipos de excepción:

- errores de usuario/negocio: mostramos el error en una ventana de diálogo. ¿Qué dice el mensaje de error? A la ventana no le interesa, sabe que cualquier acción que disparemos puede dar este tipo de error, y que el mensaje contiene información importante para el usuario. Pero no acoplamos la UI al resto de los componentes del sistema: nos da lo mismo si el número de celular debe comenzar con 15, si ya existe otro alumno con el mismo legajo o si no podemos cargar una fecha de nacimiento futura para los empleados.
- los errores de programa los catcheamos después, porque RuntimeException es más general que UserException. Hay una decisión de diseño de no envolver las excepciones chequeadas, para respetar así a quienes quieran trabajar con este tipo de excepciones. 
- mientras que la excepción de programa se "loguea" (haciendo un printStackTrace), las excepciones de negocio no tiene sentido registrarlas. Las excepciones de programa las leen los programadores, las de negocio le sirven al usuario para comprender que está tratando de usar el sistema de manera incorrecta, el mensaje de error le debe aportar una ayuda para solucionar este inconveniente y seguir adelante.
- el log default hace un printStackTrace(), esto deja en el archivo standard output la pila de mensajes donde ocurrió el error. Este archivo puede ser voluminoso, entonces
  - se suele particionar los errores por aplicación (y a veces por módulo también)
  - también se registran datos adicionales, como usuario logueado, fecha y hora en que ocurrió el error, esto por lo general se puede parametrizar sin mucho esfuerzo
  - otra cosa que se suele hacer es activar/desactivar niveles de logueo para ver cómo está funcionando una aplicación (niveles debug/info/warning/error/fatal)
  - y el ErrorViewer sabe mostrar un mensaje de error al usuario (showError), en este caso mediante una ventana de diálogo

# Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
