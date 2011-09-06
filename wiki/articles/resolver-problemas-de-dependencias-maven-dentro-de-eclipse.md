Cuando el plugin de maven dentro de eclipse no puede resolver una dependencia puede ser por varios motivos, aquí tratamos de armar una lista de cosas para revisar.

Está mal el settings.xml
------------------------

Normalmente si estuviera mal el settings entonces debería quejarse por no encontrar el parent project y no esos, pero si no le pusiste parent a tu pom entonces podría mostrar ese error. Por otro lado si tenés algún otro ejemplo de la cátedra que te anda, eso querría decir que el settings está bien.

Está mal el pom.xml
-------------------

Si tenés mal las versiones no te va a andar. Asegurate de copiar exactamente lo que dice algún pom de ejemplo.

Está mal nuestro server... creo que no es el caso, pero nunca hay que descartar esa opción.
-------------------------------------------------------------------------------------------

Está mal la conexión a Internet.
--------------------------------

Asegurate de tener Internet y que el maven/eclipse tengan acceso a ella, revisar firewalls, proxies etc.

Está mal tu repo local
----------------------

Esto es más probable de lo que podría pensarse, si todo lo anterior falla lo más lógico es que sea esto. Suele pasar cuando uno usa maven desde una conexión mala (como la de frba), intenta bajar un jar, no lo logra y se mambea: queda registrado como que no se pudo bajar y no vuelve a intentar.

Para resolver esto hay que ir al repo local y borrar la info de cache... una forma fácil es borrar todo el directorio "uqbar", pero si miran a dentro pueden ser más sutiles.

Está mal el eclipse
-------------------

El eclipse también tiene sus cachés y no es raro que se mambeen... acá no hay soluciones elegantes, darle clean al proyecto, clean al maven, restartearl el eclipse... y todas esas cosas desagradables.

En algún caso yo lo resolví haciendo un touch del archivo .classpath

Borrar parte del repo local también obliga al eclipse a refrescar sus cachés.
