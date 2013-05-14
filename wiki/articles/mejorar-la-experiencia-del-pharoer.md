Muchas veces se habla de la experiencia de usuario, de la sensación del cliente, pero ¿quién piensa en el programador? La idea es muy sencilla y es básicamente responder a la pregunta ¿Qué es lo que necesitás para que seas mas feliz programando? Particularmente todas las ideas las estamos implementando en Pharo, incluso hay varias integradas en las ultimas versiones de la imagen.

Y bueno... como estamos probando vale todo, vale simplemente implementar ese feauture que extrañas del eclipse o inventar uno nuevo.

Sugerencias dependientes del contexto
-------------------------------------

El nombre comercial es **SmartSuggestions** no sé que tan smarts sean (para mi gusto bastante poco)

**Idea:** ofrecerle al programador sugerencias basadas en el contexto en el que se encuentra mientras codifica.

**Datos técnicos:** usamos el [AST](ast.html) implementado en Pharo y el RBParser

**Código:** versión en desarrollo

    Gofer it
     url: 'http://smalltalkhub.com/mc/gisela/SmartSuggestions/main';
     package: 'ConfigurationOfSmartSuggestions';
     load. 
    ((Smalltalk globals at: #ConfigurationOfSmartSuggestions) project version: #development) load.

Podés ver una demo haciendo en: <http://www.youtube.com/watch?v=WmNKbewOXkE>
