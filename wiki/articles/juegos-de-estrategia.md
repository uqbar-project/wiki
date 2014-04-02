Modelo inicial
--------------

Se desea modelar un juego de estrategia en el cual existen unidades que se pueden atacar las unas a las otras.

Al atacar una unidad a otra, se compara el potencial ofensivo del atacante con el potencial defensivo del defensor. En caso que el primero sea mayor al segundo, la unidad atacada pierde tanta energía como la diferencia entre el potencial ofensivo y el defensivo involucrados. En otro caso, no ocurre nada.

Por ejemplo, si un atacante con 30 de potencial ofensivo ataca a un defensor con 20 de potencial defensivo, este último pierde 10 de energia.

Hay tres tipos de unidades:

-   **Guerreros** -&gt; Que pueden ser atacados o atacar. Tienen un potencial ofensivo específico cada uno y también su propio potencial defensivo. Comienzan con 100 unidades de energía.
-   **Murallas** -&gt; Que pueden ser atacados pero no atacar. Tienen un potencial defensivo que equivale a su energía sobre 20, con un mínimo que depende de cada muralla. Comienzan con 1000 unidades de energía.
-   **Misiles** -&gt; Que no pueden ser atacados pero pueden atacar. Su potencial ofensivo equivale a 100 multiplicado por la cantidad de kg de explosivos que tenga, que es propia de cada misil.

