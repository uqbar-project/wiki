---
layout: article
title: Excepciones - Resumen avanzado
categories: [errores, excepciones, resumen, consejos]
featured: true
---

# Material

Pueden ver un apunte completo [en este link](https://docs.google.com/document/d/1G0a9j-OA0rIEA5cdvEhIMbztJVo86ssvZKBK8HL9akg/edit?usp=sharing)

# Formas de manejar una excepción

## Propagarla

Es una buena opción cuando no se cómo salvar la situación, e implica
  
1. propagar la misma exception (declarando throws XXXException en la firma del método)
2. envolverla (_wrappearla_) en otra nueva, que agregará un mensaje de más alto nivel. Ej:

```java
public void enviarResumenMensual(Cliente c) {
    try {
        String resumen = this.generador.crearResumen(c);
        this.clienteMail.enviarMail(c.getEmail(), resumen);
    } catch (EmailException e) {
        throw new ProgramException("Error al enviar el resumen por mail al usuario " + c.getNombre(), e);
    }
}
```

## Tratarla

Cuando tengo un requerimiento específico como:

- Si falla el servidor de mail, intentar con una lista de otros servidores alternativos.
- Si falla el servidor de mail, esperar 5 segundos y reintentar. Realizar esto unas 5 veces. Caso que siga fallando, abortar el envío con un mensaje. 

# Buenas Prácticas

- **Nunca catchear una exception para no hacer nada**. No hacer nada también incluye el "loggearla". Es decir, catchear para loggear y seguir adelante, debería sonar muy raro. Tal vez tenga sentido eso como requerimiento en un único lugar de la arquitectura, para que no se caiga completamente la aplicación. Pero en el 99% de las veces es una hackeada.
- En general **wrappear para agregar información de contexto** (qué estaba haciendo este método al encontrar un error en otro al que llama)
  - Así al momento de fallar una operación de negocio no terminamos con un solo mensaje puntual muy específico que será dificil de comprender para el usuario/administrador/programador, como "No me pude conectar al host 12.23.22.12", si no una jerarquía de mensajes desde lo más general a lo más específico como:

```bash
Error al ejecutar el ciclo de facturación
  -> Error al facturarle al usuario numero 5963472
     -> Error al enviar el resumen mensual
        -> Error al enviar el mail
           -> No me pude conectar al host 12.23.22.12
```

No olvidar de pasar la causa original del error al wrappear (en nuestro ejemplo, la referencia a través de la variable _e_)

- Evitar try-catch dentro de otro try-catch: refactorizar la parte interna que puede fallar, llevándola a otro submétodo.
- Evitar código antes y después de un try-catch, llevando todo el código a la forma

```scala
método() {
    try {
        logica
    } catch
}
```

Es decir que el try-catch envuelva "todo" el código del método. Este va de la mano con la idea de hacer métodos más chiquitos. Al hacer una sola cosa, el método solo puede tener un tipo de falla.
