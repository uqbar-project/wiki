---
layout: article
title: Referencia de jstl
---

Para configurar JSTL pueden mirar: [Creación de un proyecto web basado en JSP y Servlets\#Cómo agregar bibliotecas adicionales](creacion-de-un-proyecto-web-basado-en-jsp-y-servlets-como-agregar-bibliotecas-adicionales.html)

# Ejemplos

El tag c:forEach de JSTL es uno de los más útiles tags para iterar sobre datos, uno de los usos más comunos es para que
se pueda generar tablas HTML que contienen resultados de una query de SQL, o de otra bbdd no sql pero que se desee representar
en forma de tabla. El siguiente ejemplo es uno sencillo que toma los datos de un bean llamado persona que wrappea una
coleccion de objetos de personas. El forEach itera sobre cada objeto en la colección generando una fila por cada uno.
 
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
        <tr class="ÃÂ${rowStyle}">
          <td>${person.name}</td>
          <td>${person.age}</td>
          <td>${person.height}</td>
        </tr>
      </c:forEach>
    </table>
  </body>
</html>
```


Material de referencia
----------------------

-   Tutorial de IBM: <http://www.ibm.com/developerworks/java/library/j-jstl0318/>
-   Otros ejemplos: <http://javarevisited.blogspot.com.ar/2012/10/jstl-foreach-tag-example-in-jsp-looping.html>

