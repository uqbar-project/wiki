El polimorfismo en el paradigma de objetos se define como **la capacidad que tiene un objeto de poder tratar indistintamente a otros que sean potencialmente distintos**. Cuando hablamos de que un objeto "trate" a otros, estamos hablando de que interactúen a través de mensajes.

Lo que nos va a interesar al programar es ver a un objeto desde un punto de vista externo (cuando mandamos mensajes) y desde un punto de vista interno (cuando implementamos el comportamiento que queremos). Al punto de vista externo, que sólo incluye el comportamiento que exhibe (los mensajes que entiende) le vamos a decir **interfaz**.

¿Y Como puede un objeto tratar indistintamente (o a partir de ahora, polimorficamente) a otros?

*Mandandole los mismos mensajes!*

Entonces, podemos decir que un objeto trata polimórficamente a otros cuando les envía a ambos exactamente los mismos mensajes, sin importarle cual es cual y ellos pueden responder ya que tienen una interfaz común.

------------------------------------------------------------------------

¿Cuántos objetos son necesarios como mínimo para que exista el polimorfismo?

*Tres: El objeto que "usa" y los (como mínimo) 2 que son "usados".*

¿Cuáles son las condiciónes necesarias para que dos objetos sean polimórficos?

'' - que un tercero quiera usarlos indistintamente''

'' - que entiendan los mensajes que ese tercero quiere enviarles''

Cabe resaltar que para que dos objetos sean polimórficos en un contexto determinado (es decir, ante el tercero, en un momento particular), es condición necesaria que entiendan los mismos mensajes. Sin embargo, para que puedan ser usados indistintamente, además de ello, deben comportarse ante tal envío de mensaje, de una forma *similar* desde el punto de vista del dominio, produciendo efectos similares, devolviendo objetos también polimórficos entre sí, etc.
