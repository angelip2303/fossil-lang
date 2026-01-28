- [ ] Mejorar la forma en la que manejamos los metadatos. Quizás con el concepto de atributos, la noción de entity pierde sentido? El problema es que en el caso de ShEx, como obtiene como debes serializar los sujetos, ya que ShEx sólo representa el vecindario de un nodo, y no a qué sujetos afecta... no?

- [ ] Tenemos que soportar alguna forma de especificar tipos para los distintos elementos, quizás algo así (rdfs:type), quizás algunos metadatos como (rdfs:label) o (rdfs:comment)
también podrían estar incluidos en este pack
  ```
  #[rdf(type = "http://xmlns.com/foaf/0.1/Person")]
  type Output = {
      #[rdf(uri = "http://example.com/id")]
      id: string,
  
      #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
      name: string,
  }
  ```

- [ ] ¿Qué pasa si un atributo type aparece junto con una shape que define rdfs:type también?

- [ ] La serialización de RDF no funciona si un record empieza por mayus

- [ ] Permitir la generación de RDF en distintos formatos de serialización

- [ ] Permitir el uso de PrefixMap o similar

- [ ] Esto no funciona, pero debería: `const ROOT = "lib/fossil-stdlib/examples/shex-provider"

type PersonData = csv!("${ROOT}/people.csv")
type Person = shex!("${ROOT}/person.shex", shape: "PersonShape")

impl ToString for PersonData {
    to_string = fn(self) -> "PersonData"
}

let csv_to_rdf = fn(row) -> {
    Person(row.name, row.age, row.name)
        |> Entity::with_id("http://example.com/person/${row}")
}

PersonData::load()
|> List::map(csv_to_rdf)
|> Rdf::serialize("${ROOT}/results.ttl")
` Sin embargo, si hago esto: `
const ROOT = "lib/fossil-stdlib/examples/shex-provider"

type PersonData = csv!("${ROOT}/people.csv")
type Person = shex!("${ROOT}/person.shex", shape: "PersonShape")

impl ToString for PersonData {
    to_string = fn(self) -> "PersonData"
}

let csv_to_rdf = fn(row: PersonData) -> {
    Person(row.name, row.age, row.name)
        |> Entity::with_id("http://example.com/person/${row}")
}

PersonData::load()
|> List::map(csv_to_rdf)
|> Rdf::serialize("${ROOT}/results.ttl")
`, Fíjate que al anotar los tipos en la función sí que funciona correctamente. Debemos darle una vuelta a como manejamos eso: debería ser obligatorio? El mensaje de error debería sugerir que debería implementar ToString? Debería inferir el tipo? Quizás el string interpolation debería exigir que implemente ToString? Por otro lado, quizás string interpolation debería ser un azúcar sintáctico para String::concat, y que String::concat debería de exigir que sus parámetros implementen ToString.


- [ ] Pensar cómo manejar records y named records cuando son iguales...


- [ ] Tenemos que pensar cómo manejamos casos en los que dos Records tengan el nombre de una columna igual y se haga join


- [ ] Explorar si `Rdf::serialize` debe devolver un DCAT
