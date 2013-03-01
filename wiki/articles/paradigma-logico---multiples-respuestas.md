Al hacer una [consulta](paradigma-logico---un-poco-de-nomenclatura-consultas.html) se puede obtener más de una respuesta. Esto está relacionado con que estamos trabajando con relaciones, no con funciones.

P.ej. si Pedro tiene como primos a Lucía, Alan y Guido; entonces la relación "ser primos" relaciona a Pedro con tres personas, entonces si consulto sobre los primos de Pedro, lo que debe pasar es que se obtengan tres respuestas, una para cada primo.

` primo(pedro,lucia).`
` primo(pedro,alan).`
` primo(pedro,guido).`

` ?- primo(pedro,Primo).`
`    Primo = lucia;`
`    Primo = alan;`
`    Primo = guido`

En funcional no tendría sentido que para la función primo existan 3 respuestas para el valor Pedro, dado que no cumpliría con la propiedad de unicidad de las funciones. La única forma de responder que tanto Lucía, Alan y Guido son primos de Pedro es mediante el uso de valores compuestos.
