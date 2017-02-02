---
layout: article
title: Reflection
---

Es un caso particular de [metaprogramación](metaprogramacion.html), donde "metaprogramamos" en el mismo lenguaje en que están escritos (o vamos a escribir) los programas. Es decir, todo desde el mismo lenguaje.

Nota de color: Inicialmente el lenguaje "pionero" en cuanto a reflection fue LISP.

El ejemplo más visible de esto es el caso de smalltalk, donde no existe una diferenciación entre IDE y nuestro programa. Ambos estan hechos en smalltalk, y de hecho viven en un mismo ambiente. Ambos estan construidos con objetos y pueden interactuar entre sí.

De hecho, muchos componentes del "IDE" Pharo son elementos de metaprogramación, y utilizan reflection para inspeccionar nuestras clases y objetos.

Tipos de reflection
-------------------

Para esto, generalmente, es necesario contar con facilidades o herramientas específicas, digamos "soporte" del lenguaje. Entonces reflection, además, abarca los siguientes items que vamos a mencionar en esta lista:

-   Introspection: se refiere a la capacidad de un sistema, de analizarse a sí mismo. Algo así como la introspección humana, pero en términos de programa. Para eso, el lenguaje debe proveer ciertas herramientas, que le permitan al mismo programa, "ver" o "reflejar" cada uno de sus componentes.
-   Self-Modification: es la capacidad de un programa de modificarse a sí mismo. Nuevamente esto requiere cierto soporte del lenguaje. Y las limitaciones van a depender de este soporte.
-   Intercession: es la capacidad de modificar la semántica del modelo que estamos manipulando, desde el mismo lenguaje.

Un MOP (MetaObject Protocol) es un framework de objetos que describe o modela un sistema de objetos. MOP sería el término correcto para lo que en java llamamos API de reflection. En realidad el API de reflection de java es un caso de MOP.

Dependiendo de la implementación y del lenguaje, el MOP puede soportar o no los tipos de reflection que enumeramos arriba: introspection, self-modification & intercession.

Introspection
-------------

Se refiere a poder obtener información acerca de los elementos de nuestro programa: clases, fields, métodos, funciones, predicados (en otros paradigmas).

La capacidad de introspection es la más común en los MOP's. Digamos que es la más "simple" de implementar, comparándolas con los otros tipos de reflection.

Veamos algunas herramientas que nos dan los lenguajes Java y Smalltalk (Pharo):

**Información estática:**

`Para obtener la clase de un objeto:`
`   en Java: getClass`
`   en Pharo: class`
`   `
`En la clase class están los métodos:`
`   en Java: getMethods, getDeclaredMethods, getFields, getConstructors, getMethod, getSuperclass`
`   en Pharo: instVariables, methodDictionary, superclass`

**Información dinámica:**

`Obtener el valor de un field:`
`   en Java: unField.get(unObjeto)`
`   en Pharo: unObjeto instVarNamed: fieldName`
`Setear el valor de un field:`
`   en Java: unField.set(unObjeto, unValor)`
`   en Pharo: unObjeto instVarNamed: fieldName put: unValor`
`Invocar un método:`
`   en Java: unMethod.invoke(unObjeto, parametro1, parametro2, ...)`
`   en Pharo: unObjeto perform: #selector withArguments: listaDeParametros`
`Crear una instancia:`
`   en Java: unConstructor.invoke(parametro1, parametro2, ...) o unaClase.newInstance()`
`   en Pharo: unaClase new    `

Self-modification
-----------------

Supongamos que queremos hacer el refactor **extract superclass**, para esto vamos a usar self-modification en Pharo, donde modificar el metamodelo usando el mismo lenguaje es algo cotidiano por ejemplo, para crear una clase usamos el template que nos provee el System Browser:

` Object subclass: #NameOfSubclass`
`   instanceVariableNames: ''`
`   classVariableNames: ''`
`   poolDictionaries: ''`
`   category: ''.`

que no es más que un mensaje que se le manda a la clase Object.

Lo que hacemos es, dada una clase, crear una super clase a partir de ella, diciéndole el nombre de le nueva clase, las variables de instancia que quiero que tenga, los métodos que quiero que tenga. Así es como nos gustaría poder usarlo:

`SimpleExtractSuperclassRefactoring `
`  extractSuperclass: #Guerrero`
`  withInstanceVariables: Soldado instVarNames`
`  andMethods: (Soldado selectors difference: #(paso fuerza))`
`  fromClass: Soldado.`

El resultado de esta prueba debería ser crear una clase Guerrero, a partir de otra clase Soldado ya existente. Guerrero va a ser superclase de Soldado y va a tener todas las variables de instancia que tiene Soldado (le podríamos especificar cuáles variables de instancia queremos ponerle, pero por simplicidad, le pasamos todas). También vamos a mover todos los métodos de Soldado, excepto paso y fuerza.

Por supuesto, nuestro refactor debe ser independiente del dominio de los guerreros. Nuestro metaprograma sería básicamente (obviando accessors):

`#SimpleExtractSuperclassRefactoring class`
`>> extractSuperclass: newSuperClassName withInstanceVariables: instanceVariableNames andMethods: selectorsToMove fromClass: aClass`
` self new`
`   target: aClass;`
`   superClassName: newSuperClassName;`
`   instanceVariableNames: instanceVariableNames;`
`   selectors: selectorsToMove; `
`   execute.`

`#SimpleExtractSuperclassRefactoring`
`>> execute`
`  self createSuperClass.`
`  self moveInstanceVariables.`
`  self moveMethods.`

`>> createSuperClass`
`  self targetSuperClass: (self target superclass`
`                                     subclass: self superClassName`
`                                     instanceVariableNames: ''`
`                                     classVariableNames: ''`
`                                     poolDictionaries: ''`
`                                     category: self target category).`
`  self targetSuperClass subclass: self target name`
`                        instanceVariableNames: self target instanceVariablesString`
`                        classVariableNames: self target classVariablesString`
`                        poolDictionaries: '' "Ahora no nos importa, pero habría que copiarlo también para que no se pierda"`
`                        category: self target category.`

`>> moveInstanceVariables`
`  self instanceVariableNames do: [:instVarName |`
`    self target removeInstVarNamed: instVarName.`
`    self targetSuperClass addInstVarNamed: instVarName. ]`

`>> moveMethods`
`  self selectors do: [:selector |`
`    self targetSuperClass compile: (self target >> selector) getSource.`
`    self target removeSelector: selector. ]`
` `

Algunos métodos de self-modification que usamos:

-   **Agregar un método:** unaClase compile: '...código del método'
-   **Borrar un método:** unaClase removeSelector: \#selector
-   **Agregar una variable de instancia:** unaClase addInstVarNamed: 'nombre de la variable'
-   **y para borrarla:** unaClase removeInstVarNamed: 'nombre de la variable'

