---
layout: article
title: json
categories: [json, web, JS, javascript]
featured: true
---

# JSON

## Introducción

JSON (_JavaScript Object Notation_) es un estándar para expresar estructuras de datos en un subset del lenguaje javascript. Se hizo popular en los últimos tiempos como el estándar de formato para transferir información entre sistemas. El JSON es una estructura arbórea, donde cada objeto es un mapa o diccionario.

Facebook, Twitter, Foursquare proveen una forma de acceder a su información (contactos, tweets, y lugares, respectivamente) que nos retorna la información en formato JSON. La idea es parecida a la idea de XML (asumiendo que ya conocen esta idea), pero menos verborrágico.

## Ejemplo de una persona

Ejemplo de una persona en formato JSON:

- Un objeto es un conjunto de "clave-valor" entre llaves.
- Las claves tienen nombres en strings (entre comillas)
- Los valores pueden ser strings o números, o bien otras estructuras.
- "telefonos" tiene como valor una lista o array, que se escribe con literales "corchetes". Y elementos separados por comas.
- "mascota" tiene como valor otro objeto JSON, que a su vez tiene sus propios atributos como clave-valor.

```bash
{
  "nombre": "Juan",
  "apellido": "Pérez",
  "edad": 45,
  "telefonos": [
    47574933,
    29298122,
    88384831
  ],
  "mascota": {
    "nombre": "Colita",
    "tipo": "perro"
  }
}
```

# Material complementario

- [Tutorial de JSON](http://geekytheory.com/json-i-que-es-y-para-que-sirve-json/)
