---
layout: article
title:  como convertir mi aplicacion arena en un  exe 
---

De vez en cuando surge una pregunta, enunciada, mas o menos así: "Ahora que tengo mi aplicación Arena, ¿cómo hago pera crear un .exe?".

Primero, "qué hacer si a la aplicación de conversión de unidades la querés convertir en un .exe", no es una pregunta con una respuesta fácil.

Primero tenés que preguntarte qué es un archivo .exe. La respuesta autámtica es: un archivo ejecutable (executable), es decir, que tiene un programa que podés ejecutar. Pero la realidad es un poco más compleja: en particular un .exe un programa ejecutable binario nativo para el sistema operativo Windows.

Segundo, reconozcamos qué es una aplicación Arena: es una aplicación para la máquina virtual de Java (JVM). Es decir también es un programa binario ejecutable, pero no es nativo para Windows, sino que se ejecuta sobre la JVM (que en tanto tiene un .exe para ejecutarse sobre Windows). Nosotros no nos tomamos el trabajo de hacer a nuestra aplicación fácilmente distribuible (la ejecutabamos desde eclipse), pero hacerlo es trivial: hay que decirle a Eclipse o a Maven que construya un empaquetado .jar (el mismo tipo de empaquetado que Hibernate o Arena usan para distribuirse).

Entonces la respuesta rápida es: no se puede, son cosas distintas. Si querés ejecutar una aplicación Arena, tenés que ejecutar la JVM y pasale como parámetro de ejecución la aplicación Arena.

Ahora bien, probablemente lo que a vos te interesa para esta pregunta no es construir realmente un .exe, sino simplemente un archivo ejecutable (no necesariamente binario, no necesariamente para Windows), que se pueda ejecutar fácilmente por un usuario común y corriente (con un doble click sobre el archivo, por ejemplo).

Eso sí tiene una respuesta satisfactoria: en Linux o Mac, donde es extremadamente fácil hacer archivos ejecuables, es necesario tan sólo escribir un pequeño script que ejecute la JVM y le pase tu programa. Luego el sistema operativo te permite ejecutar fácilmente al script,con lo que lograste el objeto. En Windows hay que mentirle un poco más: hay programas empaquetadores a los que les pasás tu aplicación para la JVM y te crean un .exe que en realidad.... es un simple programa nativo que lo único que hace es ejecutar la JVM y pasarle tu programa Java por detrás :).

Esta es la misma estrategia que utiliza .net y otras tecnologías. Moraleja, hoy en día aún en Windows pocos programas son REALMENTE nativos (es decir, programas compilados para tu CPU y sistema operativo, como lo hacés en Algoritmos o en Operativos), sino que tan solo tienen este componente nativo que inicia la máquina virtual de la tecnología que uses.

Y finalmente, hay algunas opciones más: <http://www.excelsior-usa.com/articles/java-to-exe.html>
