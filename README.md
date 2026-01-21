# TODO

## Sigue habiendo algun bug con el autocompletado de FieldSelectors, además, los field selectors no parece que resuelvan tipos correctamente

## En el caso de que se esté usando named arguments en una función, no hay coloreado de sintaxis correcta, y el hover parece no esar funcionando super bien

## Al hacer hover en un string interpolation, muestra string, aunque no sea string el campo concreto

## El hover de: type OrderDetail = {
    #[rdf(uri = "https://example.com/order/order_date")]
    order_date: string,
    #[rdf(uri = "https://example.com/order/quantity")]
    quantity: int,
    #[rdf(uri = "https://example.com/order/product_name")]
    product_name: string,
    #[rdf(uri = "https://example.com/order/customer_name")]
    customer_name: string
} Se muestra raro

## Eliminar debugs

##  let atom = function
    .or(application)
    .or(unit)
    .or(literal)
    .or(list)
    .or(block)
    .or(record)
    .or(placeholder)
    .or(path) Mejor choice?
 

## Pensar cómo manejar records y named records cuando son iguales...

## Permitir la anotación de tipos en parámetros

## Constantes: `const ROOT = ""` (Sólo pueden ser tipos primitivos (estáticos))

## Soporte para argumentos provenientes de la terminal: `Args`

## Explorar si `Rdf::serialize debe devolver un DCAT`

## Dar soporte para Excel

## Properly handle imports

Import statements should be properly handled. This should also be included in the LSP.

## Descubrir cómo se instala el LSP
