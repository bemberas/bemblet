﻿module Tests

open System
open Xunit


open Ast
open FParsec.CharParsers

let assertParseError template =
    match Parser.parse template with
    | ParserResult.Success (result, state, pos) -> failwith "Expected parser to fail"
    | ParserResult.Failure (message, error, state) -> Assert.True(true)

let assertParsesTo template expected =
    match Parser.parse template with
    | ParserResult.Success (result, state, pos) -> Assert.Equal(expected, result)
    | ParserResult.Failure (message, error, state) -> failwith message


[<Fact>]
let ``Empty template yields empty component list`` () =
    assertParsesTo
        ""
        { components = []; }

[<Fact>]
let ``Single expression`` () =
    assertParsesTo
        "{{foo:string:This is the description}}"
        {   components = [
                Expr {
                    symbol = "foo";
                    kind = { name = "string"; constraints = []; };
                    description = "This is the description";
                }
            ];
        }

[<Fact>]
let ``Verbatim text only`` () =
    assertParsesTo
        "foo bar baz"
        { components = [ Text "foo bar baz" ] }

[<Fact>]
let ``Expression in middle of text`` () =
    assertParsesTo
        "begin{{fourteen:thirtyTwo:One hundred and thirty six}}end"
        {   components = [
                Text "begin"
                Expr {
                    symbol = "fourteen";
                    kind = { name = "thirtyTwo"; constraints = []; };
                    description = "One hundred and thirty six";
                }
                Text "end"
            ];
        }

[<Fact>]
let ``Free-form closing expression bracket is parsed as text`` () =
    assertParsesTo
        "asd }} asd"
        {
            components = [
                Text "asd }} asd"
            ]
        }

[<Fact>]
let ``Empty identifiers are not allowed`` () =
    assertParseError "{{:a:}}"
