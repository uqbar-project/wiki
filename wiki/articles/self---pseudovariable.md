---
layout: article
title: Self   pseudovariable
---

**self** es una pseudovariable que **siempre** referencia al objeto receptor del mensaje que hizo que se evalúe el método que estamos analizando en un momento dado. Tené en cuenta que no es posible asignar self, esta referencia es manejada por la máquina virtual, no por el programador.

Sirve para que el objeto receptor pueda mandarse un mensaje a sí mismo dentro de la implementación del método que se está ejecutando o para que, al mandar un mensaje a otro objeto que necesite colaborar con él, pueda pasarse a sí mismo por parámetro.

Ejemplo en Wollok:

Si tenemos este envío de mensaje:

`pepita.vola(10)`

y pepita está definida de la siguiente forma:

```
object pepita {
  var energia = 100
  
  method energia(){
    return energia
  }
  
  method energia(nuevaEnergia){
    energia = nuevaEnergia
  }
  
  method vola(unosKms){
    self.energia(self.energia() - unosKms)
  }
}
```

Cuando se le mande el mensaje para volar 10 kilómetros a pepita, se va a ejecutar el método que envía los mensajes para obtener y modificar la energía a self, o sea que pepita va a ser quien reciba ambos mensajes.

Como se mencionó antes, en cualquier método es perféctamente válido parametrizarse a uno mismo, por ejemplo:

```
object pepita {
 method teEntrena(unEntrenador) {
   self.come(50)
   unEntrenador.entrenaA(self)
 }
}
```

Los mismos ejemplos anteriores, pero en Smalltalk:

Envío de mensaje inicial:

`pepita vola: 10.`

Método que se ejecutaría:

```
>> vola: unosKms
   self energia: self energia - unosKms
```

Parametrización de self:

```
>> teEntrena: unEntrenador
   self come: 50.
   unEntrenador entrenaA: self.
```
