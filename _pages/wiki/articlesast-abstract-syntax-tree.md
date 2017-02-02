---A esto hay que darle forma --- AST simplemente es un arbol que lo que hace es representar un programa (o parte de él), se organiza en nodos y puede haber nodos que contengan otros (nada loco).

Lo interesante es usarlo, para armarlo ya está hecho, esta bueno aprender a usarlo... Aka... como obtenerlo, como visitarlo, que sabe hacer cada nodo, como agregarle comportamiento.

Si abris Pharo vas a ver que los nodos son RB<loQueSea>Node, ejemplo RBVariableNode, RBSequenceNode, RBMessageSendNode (...).

Si queres ver como armas un AST de un cacho de codigo tenes RBParser parse: unString y te devuelve un nodo del ast (el parente, un program node si parseo bien).

No encontre mucho que leer.. a ver vi una presentacion y lei algo a media que me paso ducasse, y despues para ver como se puede jugar con poder sobre el AST lei el paper de Helvetia y otro de SmallLint (este ultimo es bien concreto sobre reglas y transformaciones).

Despues me puse a jugar y a reconocer nodos segun lo que esta seleccionado... facil porque abri el AltBrowser que ya lo hacia.

Despues nada... browsear el codigo para entender un poco como se implementa... si encontras algo mas organizado decime... (en otro mail te reenvio lo que me mando ducasse para leer)

Reng10aEmbeddingLanguages.pdf

Reng10bDomainSpecificProgramChecking.pdf
