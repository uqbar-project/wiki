---
layout: article
title: Warning  singleton variables
---

Las singleton variables son variables que vos escribiste en tu predicado pero no se usan. Por ejemplo:

"Soy feliz si tengo algun amigo"

```prolog
esFeliz(Alguien) :- amigoDe(Alguien,**UnAmigo**).
```

Ahí la variable UnAmigo no la usás en otro lado, por lo que te dice que es Singleton. Como a vos no te interesa "usar" a ese UnAmigo (Sólo te interesa saber si existe, no quién es), podés poner una variable anónima:

```prolog
esFeliz(Alguien) :- amigoDe(Alguien,**_**).
```

Y así ya no te chilla.

Ojo, a veces pasa que escribís mal una variable, entonces te dice "eh, singleton, ésta no la estás usando en otro lado" Por ejemplo:

```prolog
esFeliz(**Alguin**) :- amigoDe(**Alguien**,_).
```

Y ahí sólo tenés que arreglar tu error de tipeo.

En resumen, si tenés un Warning,

-   puede ser que tu programa igual ande perfecto (en el primer caso la variable anónima funciona igual que una común), ó bien
-   puede ser que por un error de tipeo tu programa no ande (qué bueno que nos avise).
-   En general, los warnings nos deberían hacer pensar si escribimos bien las variables, y si lo hicimos bien, deberíamos entonces revisar la lógica del predicado (pifiarla en eso es un error más grave)

