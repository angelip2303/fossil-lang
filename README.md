- [ ] ¿Qué pasa si un atributo type aparece junto con una shape que define rdfs:type también? Creo que lo que deberíamos de hacer es emitir un warning, indicando que se ignorará el del atributo type.

- [ ] Explorar cómo manejar el tema de los duplicados
- [ ] ¿Qué pasa si dos sujetos son iguales? Hay que tener en cuenta también el concepto de RML de rr:constant, que puede aparecer en un subjectMap, predicateMap y objectMap:
@prefix rr: <http://www.w3.org/ns/r2rml#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.com/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rml: <http://semweb.mmlab.be/ns/rml#> .
@prefix ql: <http://semweb.mmlab.be/ns/ql#> .
@base <http://example.com/base/> .

<TriplesMap1> a rr:TriplesMap;
    
  rml:logicalSource [ 
    rml:source "student.csv";
    rml:referenceFormulation ql:CSV
  ];

  rr:subjectMap [ 
    rr:constant ex:BadStudent;  
    rr:graphMap [ rr:constant <http://example.com/graph/student> ];
  ];

  rr:predicateObjectMap [ 
    rr:predicateMap [ rr:constant ex:description ]; 
    rr:objectMap [ rr:constant "Bad Student"; ]
  ].
  Lo que me preocupa de esto es que si para evitar serializar sujetos repetidos lo que hacemos es usar un HashSet por ejemplo, entonces en memoria estaríamos almacenando todos los sujetos, lo que podría ser un problema si tenemos muchos sujetos. No sé si es mejor tener una operación que sea distinct y dejar que polars por debajo sea el que lo maneje

- [ ] Permitir el uso de PrefixMap o similar

- [ ] Manejar el tipo opcional



- [ ] Tenemos que pensar cómo manejamos casos en los que dos Records tengan el nombre de una columna igual y se haga join

- [ ] Explorar si `Rdf::serialize` debe devolver un DCAT
