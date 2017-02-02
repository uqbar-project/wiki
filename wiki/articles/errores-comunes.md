---
layout: article
title: Errores comunes
---

Contenido del tag "web-app"
---------------------------

`The content of element type "web-app" must match`
`"(icon?,display-name?,description?,distributable?,context-param*,`
`filter*,filter-mapping*,listener*,servlet*,servlet-mapping*,session-config?,mime-mapping*,welcome-file-list?,error-page*,`
`taglib*,resource-env-ref*,resource-ref*,security-constraint*,login-config?,security-role*,env-entry*,ejb-ref*,ejb-local-ref*)".`

El mensaje quiere decir que el contenido web-app es inválido, está mal formado.

Para entender el mensaje, en la lista entre paréntesis están los posibles tags "hijos" de webapp, para solucionarlo habría que fijarse si tienen algo que esté dependiendo de web-app y que no se ajuste a esa lista.

Además:

-   Tienen que estar en el orden que están en esa lista.
-   Los que tienen ? son opcionales pero pueden 0 o una veces,
-   los que tienen \* pueden estar 0 o más veces.

Un error muy común podría ser al tener varios servlets poner cada servlet con su servlet mapping y el estándar exige poner primero todos los servlets y recién después todos los servlet mappings.

En este caso puntual, es muy posible tener esa validación en algún editor y que la aplicación funcione de todas maneras, porque el tomcat no es estricto al chequear el formato del web.xml. La recomendación es de todas formas ajustarse al estándar.
