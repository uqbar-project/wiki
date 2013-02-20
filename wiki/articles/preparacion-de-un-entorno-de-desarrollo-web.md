Herramientas de desarrollo Web
------------------------------

Vamos a necesitar instalar

-   un web server
-   un browser con capacidades de debug
-   un plugin para nuestro IDE (Eclipse u otro) que permita administrar nuestro web server

Web Server Tomcat
-----------------

### Instrucciones de instalación

Descargarlo de [acá](http://tomcat.apache.org/download-70.cgi). Recomendamos utilizar la última versión estable disponible; ojo si estás usando el plugin Sysdeo que la versión del Tomcat 7.0 puede tener algunos problemas de instalación, si es así lo mejor es instalar el wtp que es el plugin recomendado por nosotros.

Descomprimirlo en una ubicación adecuada. Puede ser cualquier ubicación, aunque recomendamos usar una que no tenga espacios, por ejemplo: En windows pueden usar C:/tools/tomcat-version En linux/unix pueden usar /opt/tomcat o bien /home/algun-usuario/tomcat-version A la carpeta donde descomprimimos el tomcat la vamos a denominar $TOMCAT\_HOME, ej. /home/nico/Applications/apache-tomcat-6.0.20.

Si lo vamos a usar desde el eclipse con eso es suficiente, si alguien quisiera un uso más extensivo del tomcat tal vez tenga ganas de configurarlo como servicio.

Editar $TOMCAT\_HOME/conf/tomcat-users.xml y agregar un usuario con el rol de manager, para eso hay que agregar estas dos líneas dentro del tag tomcat-users.

`   `<role rolename="manager"/>
`   `<user username="admin" password="admin" roles="manager"/>

(Si el tomcat va a estar en una máquina con acceso público probablemente quieran ponerle una password más segura que esa!)

Browser o Navegador
-------------------

Si no lo tenés instalado podés usar:

-   [Google Chrome](http://www.google.com/intl/es/chrome/)
-   [Mozilla Firefox](http://www.mozilla.org/es-AR/firefox/new/), al que le podés instalar [firebug](http://getfirebug.com/) (si estás en Linux recordá ejecutar sudo apt-get install firefox)
-   [Internet Explorer](http://windows.microsoft.com/es-ES/internet-explorer/download-ie)
-   [Safari](http://www.apple.com/es/safari/)
-   [Opera](http://www.opera.com/download/)

Plugin para tu IDE
------------------

-   [WTP](http://www.eclipse.org/webtools/), Web Tools Platform, que se descarga con la versión EE de Eclipse o bien [aquí](http://eclipse.org/webtools/releases/3.4.1/)
-   [Sysdeo](http://www.eclipsetotale.com/tomcatPlugin.html)

Cómo probar la instalación
--------------------------

Levantar el servidor, hay varias formas de hacerlo: Si vas a usar el tomcat desde el Eclipse probablemente lo levantes usando Wtp o Sysdeo, en ese caso te recomiendo que primero instales esas herramientas y luego pruebes todo junto.

También se puede levantar el tomcat por fuera del entorno de desarrollo. En Ubuntu tenés que hacer: sudo /etc/init.d/tomcat6 start

En Windows hay que ir a servicios (el shortcut es Win + R: "services.msc"), buscar el Tomcat y darle Start. Otra opción es abrir una ventana de comandos (Win + R: "cmd"), ir al $TOMCAT\_DIR\\bin (o sea, la carpeta bin donde está instalado el Tomcat) y ejecutar "startup".

Dos aplicaciones deberíamos poder ver ya andando luego de haber hecho eso: La página home <http://localhost:8080/>, es lo más fácil para ver si anda. La página de administración: <http://localhost:8080/manager/html>, ahí pueden ver qué aplicaciones tienen levantadas en el tomcat y realizar algunas tareas administrativas. Para poder abrir esa página tienen que loguearse con el usuario y password que ingresaron al configurar el archivo $TOMCAT\_HOME/conf/tomcat-users.xml.
