---
layout: article
title: Method missing
---

Method missing (o Does not understand en Smalltalk) es un punto de intercepción en el method lookup que ofrecen algunos lenguajes para definir código a ejecutar si el objeto receptor no entiende el mensaje recibido. Si el method lookup falla, el objeto receptor recibirá el mensaje method\_missing que en Object lanza un error por no entender el mensaje, pero qué pasa si redefinimos este mensaje?

Poder interceptar el method lookup de esta forma y redefinir lo que debería suceder en caso de que un objeto de la clase A no entienda el mensaje recibido es una herramienta muy poderosa ya que permite armar definiciones genéricas para cualquier mensaje (o para un amplio conjunto que nosotros estemos interesados).

Algunos ejemplos de uso:

-   en ocasiones es útil usar un objeto que ante cualquier mensaje no haga nada, pero tampoco explote (un deaf object), en este caso la implementación es trivial ya que un method\_missing vacío hará que la interfaz de nuestro objeto sea infinita.
-   se puede usar para hacer APIs que, en base a algún patrón del nombre del mensaje, se pueda ejecutar una lógica particular. Por ejemplo, en Ruby on Rails se pueden consultar todos los objetos persistidos que cumplan un cierto criterio en base a los atributos de los mismos con un mensaje a la clase *find\_by* que recibe un diccionario:

`Person.find_by(user_name: user_name, password: password)`

Pero también se puede usar de la siguiente forma:

`Person.find_by_user_name_and_password(user_name, password)`

-   si se quiere implementar un [decorator](http://sourcemaking.com/design_patterns/decorator) para lo cual el decorador tendrá muchos métodos que sean sólo delegar en el decorado el mismo mensaje que recibió, podría resolverse el dispatch de forma genérica en el method\_missing y sólo definir aquellos métodos en los cuales sí existe alguna lógica propia que aportar.

Supongamos que queremos hacer el method\_missing para nuestro decorador, podría ser algo como:

`def method_missing(symbol, *args, &block)`
`   if @decorado.responds_to?(symbol)`
`     @decorado.send(symbol, *args)`
`   else`
`     super`
`   end`
`end`

De esa forma si el objeto decorado entiende el mensaje, se lo mandará, de lo contrario ejecutará method\_missing de la superclase.
