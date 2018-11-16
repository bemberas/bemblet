module Parse

open FParsec

let parse (template:string) : Ast.Document =
    { components = []; }