
- [ ] Parece que annotations y metadata es como que los dos hacen referencia a lo mismo



- [ ] En mi opinión, es probable que estemos usando más cosas de terceros de las que deberíamos

- constructor.rs debería de ser unicamente azucar sintactico para la instanciación de records: Es
  decir, type Name = { name: string }, se podría construir así: let name: Name = { name = "John" }
  o así: let name = Name("John")

- [ ] Va fuera todo el tema de extensiones, asegurarse de ello

- [ ] fn parse_subject_pattern(subject_uri: &str) parece que hardcodea el transformar string interpolation, string interpolation deberia de ser syntax sugar para String::concat, no?

- [ ] Comprobar si List::join funciona correctamente

- [ ] Con una embedding API puede ser que pudiesemos quitar fossil-stdlib como un crate de Rust, y trabajar directamente con archivos .fossil?

- [ ] En general el tema de los errores se puede mejorar usando thiserror. Además, no sé si lo de internar los errores es la mejorar manera de manejarlo (al menos con los mensajes)

- [ ] Mejorar la forma en la que manejamos los metadatos. Quizás con el concepto de atributos, la noción de entity pierde sentido? El problema es que en el caso de ShEx, como obtiene como debes serializar los sujetos?

- [ ] Tenemos que soportar alguna forma de especificar tipos para los distintos elementos, quizás algo así:
  ```
  #[rdf(type = "http://xmlns.com/foaf/0.1/Person")]
  type Output = {
      #[rdf(uri = "http://example.com/id")]
      id: string,
  
      #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
      name: string,
  }
  ```

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

- [ ] En el caso de que se esté usando named arguments en una función, no hay coloreado de sintaxis correcta, y el hover parece no esar funcionando super bien

- [ ] Pensar cómo manejar records y named records cuando son iguales...

- [ ] Tenemos que pensar cómo manejamos casos en los que dos Records tengan el nombre de una columna igual y se haga join



## Soporte para argumentos provenientes de la terminal: `Args`

## Explorar si `Rdf::serialize debe devolver un DCAT`

## Dar soporte para Excel
