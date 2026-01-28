- [ ] ¿Qué pasa si un atributo type aparece junto con una shape que define rdfs:type también? Creo que lo que deberíamos de hacer es emitir un warning, indicando que se ignorará el del atributo type.


- [ ] Qué pasa si cometemos un error a la hora de escribir los atributos: #[rdf(id = "_:students{ID}", with = ["ID"])] en lugar de #[rdf(id = "_:students${ID}", with = ["ID"])]. Qué pasa realmente con las templates de las columnas? Estamos recreando la sintaxis de string interpolation, pero realmente no es string interpolation


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
`, Fíjate que al anotar los tipos en la función sí que funciona correctamente. Debemos darle una vuelta a como manejamos eso: debería ser obligatorio? El mensaje de error debería sugerir que debería implementar ToString? Debería inferir el tipo? Quizás el string interpolation debería exigir que implemente ToString?


- [ ] Pensar cómo manejar records y named records cuando son iguales...


- [ ] Tenemos que pensar cómo manejamos casos en los que dos Records tengan el nombre de una columna igual y se haga join


- [ ] Explorar si `Rdf::serialize` debe devolver un DCAT
