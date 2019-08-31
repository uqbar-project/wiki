---
layout: article
title: Arena. Manejo de transacciones.
categories: [arena, mvc pesado, transaccion, objetos transaccionales]
featured: true
---

# Introducción

Si estamos modificando los datos de una entidad, es frecuente hacerlo en un formulario que tiene dos acciones posibles: aceptar o cancelar los cambios.

![edicion-1](/img/wiki/arena_transaction.png)

Cuando la UI tiene binding, cualquier modificación que haga el usuario impacta directamente en el modelo. ¿Qué alternativa tenemos entonces para volver atrás los cambios si el usuario desiste la acción?

# Sincronización mediante repositorios persistentes

Si estamos en un esquema distribuido, una opción puede ser que nuestro repositorio trabaje con un **medio persistente**, con dos objetivos:

- sincronizar la información de las distintas sesiones de usuario
- proveer un ambiente donde las operaciones son transaccionales (todas las actualizaciones ocurren al mismo tiempo o si falla alguna se cancelan las restantes)

En ese caso cuando el usuario presiona el botón Aceptar se envía un mensaje al repo para que actualice el objeto del formulario, caso contrario el repo no recibirá ninguna notificación. Cuando un usuario en otra VM diferente quiere actualizar el mismo cliente, el objeto repositorio toma la información del medio persistente para recrear los datos del cliente actualizados. El medio persistente actúa como "la única fuente de verdad", y mientras que los objetos que están en el ambiente son simplemente un _buffer_ o estado temporal antes de ser almacenados en el medio.

![two_users](/img/wiki/arena_transactions_2.png)

# Trabajo en una única VM con binding

Si en nuestra aplicación el repositorio considera la VM de objetos como la _single source of truth_, el mecanismo de binding puede traer efectos colaterales: **cada vez que el usuario modifique el nombre, eso tiene un impacto inmediato en el modelo** y tenemos que pensar qué debemos hacer si el usuario quiere cancelar la operación de edición para que efectivamente se deshagan los cambios.

![single_vm](/img/wiki/arena_transactions_3.png)

## Opción 1: Manejo manual de los cambios

La primera alternativa consiste en generar una copia del objeto original y asociarlo como modelo de nuestra ventana de modificación.

- al cancelar no necesitamos hacer nada, porque los cambios se hicieron contra un objeto que no está asociado al repositorio
- al aceptar, pisamos el objeto original (referenciado desde el repositorio) con los nuevos atributos del objeto copia.

![copy](/img/wiki/arena_transactions_4.png)

## Opción 2: Elementos transaccionales de Arena

Arena propone un esquema para no tener que resolver esto manualmente:

- todos los objetos que sean modelos de una vista se anotan como `@Transactional`, para indicar a Arena que participa dentro de una transaccción. Esto incluye a los objetos de dominio y también a los objetos que modelan un caso de uso (que llamamos application model, se explican en el formulario de búsqueda).
- como además queremos que disparen notificaciones a las vistas y demás interesados, debemos mantener la annotation `@Observable`
- también existe la anotación `@TransactionalAndObservable` (cualquiera de las dos variantes funciona exactamente igual)
- la vista debe heredar de `TransactionalDialog<T>`

Y con esto nos alcanza, Arena utiliza la vista para delimitar el alcance de una transacción: cuando el usuario presiona el botón Aceptar se finaliza (`commit`). En caso de error, o de presionar el botón Cancelar, la transacción se deshace (`rollback`), y los cambios se pierden.

El lector interesado puede consultar [el ejemplo de los celulares](https://github.com/uqbar-project/eg-celulares-ui-arena-xtend) que trabaja automáticamente la transacción.

# Links relacionados

- [Temario Algoritmos III](algo3-temario.html)
