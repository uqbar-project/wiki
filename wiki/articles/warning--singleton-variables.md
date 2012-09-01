Las singleton variables son variables que vos escribiste en tu predicado pero no se usan. Por ejemplo:

"Soy feliz si tengo algun amigo"

`esFeliz(Alguien) :- amigoDe(Alguien,`**`UnAmigo`**`).`

Ahí la variable UnAmigo no la usás en otro lado, por lo que te dice que es Singleton. Como a vos no te interesa "usar" a ese UnAmigo (Sólo te interesa saber si existe, no quién es), podés poner una variable anónima:

`esFeliz(Alguien) :- amigoDe(Alguien,`**`_`**`).`

Y así ya no te chilla.

Ojo, a veces pasa que escribís mal una variable, entonces te dice "eh, singleton, ésta no la estás usando en otro lado" Por ejemplo:

`esFeliz(`**`Alguin`**`) :- amigoDe(`**`Alguien`**`,_).`

Y ahí sólo tenés que arreglar tu error de tipeo.
