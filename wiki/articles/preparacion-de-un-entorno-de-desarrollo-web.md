---
layout: article
title: Preparacion de un entorno de desarrollo web
---

# Herramientas de desarrollo Web

Vamos a necesitar instalar

-   un web server
-   un browser con capacidades de debug
-   plugins para nuestro IDE (Eclipse u otro) que permita administrar nuestro web server

# Web Server Tomcat

## Instrucciones de instalación

Descargarlo de [acá](http://tomcat.apache.org/index.html), en general te recomendamos bajarte la última versión disponible.

Descomprimirlo en una ubicación adecuada. Puede ser cualquier ubicación, aunque recomendamos usar una que no tenga espacios, por ejemplo: En windows pueden usar C:/tools/tomcat-version En linux/unix pueden usar /opt/tomcat o bien /home/algun-usuario/tomcat-version A la carpeta donde descomprimimos el tomcat la vamos a denominar $TOMCAT\_HOME, ej. /home/nico/Applications/apache-tomcat-7.0.20.

Si lo vamos a usar desde el eclipse con eso es suficiente, si alguien quisiera un uso más extensivo del tomcat tal vez tenga ganas de configurarlo como servicio.

Editar $TOMCAT\_HOME/conf/tomcat-users.xml y agregar un usuario con el rol de manager, para eso hay que agregar estas dos líneas dentro del tag tomcat-users.

```xml
   <role rolename="manager"/>
   <user username="admin" password="admin" roles="manager"/>
```   

(Si el tomcat va a estar en una máquina con acceso público probablemente quieran ponerle una password más segura que esa!)

<!-- -->

# IDE

Recomendamos que te instales la versión "Eclipse IDE for Java EE Developers" (EE es Enterprise Edition) en <http://www.eclipse.org/downloads/>. Eso evita que tengas que instalarte en forma separada el plugin [WTP](http://www.eclipse.org/webtools/), Web Tools Platform (o si preferís, tenés que descargarte el Eclipse Classic para luego descargarte [aquí](http://eclipse.org/webtools/releases/3.8.2/) el plugin).

# Plugins para tu IDE

-   El M2E y el WTP vienen con la instalación default de Eclipse EE
-   Pero para que trabajen en forma sincronizada, necesitamos agregar el plugin "m2e-wtp", esto se hace ingresando al Eclipse Marketplace, buscando "m2e-wtp" e instalando el plugin "Maven integration for Eclipse WTP". Luego restartear el IDE.

# Cómo probar la instalación

Levantar el servidor, si lo hacés dentro del entorno:

-   Ir a la vista "Servers". Dentro del eclipse: Window &gt; Show view &gt; Other &gt; Servers. Hay que configurar un Server en dicha vista: click derecho sobre la vista &gt; New &gt; Server.
    -   Elegir el server que se quiere usar (la cátedra recomienda Apache Tomcat 7) y luego Next
    -   Luego configurar el path a la carpeta raíz donde está instalado el Server, por ejemplo "/usr/local/tomcat" (no usar ningún subdirectorio lib ni bin, el raíz)
    -   Opcionalmente se puede elegir la JVM que ejecutará el tomcat. Es importante no utilizar una JRE, sino una JDK.
    -   Una vez creado, hacer doble click sobre el Server &gt; en la parte de Server Location &gt; Use Tomcat installation (takes control of Tomcat Installation).
    -   Luego de configurado el server, levantamos el mismo haciendo click derecho sobre el mismo &gt; Debug (Start también es una opción válida). En la consola del eclipse debería aparecer una línea como:

   bash
... mensajes varios de inicialización del Tomcat ...
INFO: Server startup in 798 ms
       
   
También se puede levantar el tomcat por fuera del entorno de desarrollo. En Ubuntu tenés que hacer: sudo /etc/init.d/tomcat6 start

En Windows hay que ir a servicios (el shortcut es Win + R: "services.msc"), buscar el Tomcat y darle Start. Otra opción es abrir una ventana de comandos (Win + R: "cmd"), ir al $TOMCAT\_DIR\\bin (o sea, la carpeta bin donde está instalado el Tomcat) y ejecutar "startup".

Dos aplicaciones deberíamos poder ver ya andando luego de haber hecho eso: 

La página home 

```html
http://localhost:8080/
```

es lo más fácil para ver si anda. 


La página de administración: 

```html
http://localhost:8080/manager/html
```

ahí pueden ver qué aplicaciones tienen levantadas en el tomcat y realizar algunas tareas administrativas. Para poder abrir esa página tienen que loguearse con el usuario y password que ingresaron al configurar el archivo $TOMCAT\_HOME/conf/tomcat-users.xml.

# Por último

Si estás trabajando en un lenguaje diferente a Java no te olvides de instalar los plugins con las extensiones para dicho lenguaje.

# Cómo empezar

-   Tutorial sobre html: <http://www.w3schools.com/html/>
-   Manual de referencia para manejar la estética a través de los .css: <http://www.w3schools.com/css/default.asp>

