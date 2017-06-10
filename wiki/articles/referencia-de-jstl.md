---
layout: article
title: Referencia de jstl
---

Para configurar JSTL pueden mirar: [Creación de un proyecto web basado en JSP y Servlets\#Cómo agregar bibliotecas adicionales](creacion-de-un-proyecto-web-basado-en-jsp-y-servlets.html)

# Ejemplos

En este ejemplo vemos cómo se genera una tabla o grilla de personas, en base a un conjunto de datos.

-   El conjunto de datos se encuentra en el bean people, a partir de la colección people
-   Cada uno de los elementos de la colección es un bean persona, que tiene como atributos
    -   name: nombre
    -   age: edad
    -   height: peso

El forEach itera sobre cada objeto en la colección generando una fila por cada uno.
 
```xml
<?xml version="1.0" encoding="UTF-8" ?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page" 
  xmlns:c="http://java.sun.com/jsp/jstl/core" 
  version="2.0">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>c:forEach Example</title>
  </head>
  <body>
    <table>
      <c:forEach var="person" items="${people.people}">
        <tr>
          <td>${person.name}</td>
          <td>${person.age}</td>
          <td>${person.height}</td>
        </tr>
      </c:forEach>
    </table>
  </body>
</html>
```

Se puede mejorar esto introduciendo el tag c:choose, con el que se puede aplicar banding a la tabla, con lo que lo hace 
más simple de visualizar. 

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page" 
  xmlns:c="http://java.sun.com/jsp/jstl/core" 
  version="2.0">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>c:forEach Example</title>
  </head>
  <body>
    <table>
      <c:forEach var="person" items="${people.people}" varStatus="rowCounter">
        <c:choose>
          <c:when test="${rowCounter.count % 2 == 0}">
            <c:set var="rowStyle" scope="page" value="odd"/>
          </c:when>
          <c:otherwise>
            <c:set var="rowStyle" scope="page" value="even"/>
          </c:otherwise>
        </c:choose>
        <tr class="${rowStyle}">
          <td>${person.name}</td>
          <td>${person.age}</td>
          <td>${person.height}</td>
        </tr>
      </c:forEach>
    </table>
  </body>
</html>
```

<!-- -->

# Material de referencia

-   Tutorial de IBM: <http://www.ibm.com/developerworks/java/library/j-jstl0318/>
-   Otros ejemplos: <http://javarevisited.blogspot.com.ar/2012/10/jstl-foreach-tag-example-in-jsp-looping.html>

