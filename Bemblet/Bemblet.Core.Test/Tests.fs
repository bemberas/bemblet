module Tests

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
        { components = [ Verbatim "foo bar baz" ] }

[<Fact>]
let ``Verbatim text interleaved with expressions`` () =
    assertParsesTo
        "begin{{fourteen:thirtyTwo:One hundred and thirty six}}end"
        {   components = [
                Verbatim "begin"
                Expr {
                    symbol = "fourteen";
                    kind = { name = "thirtyTwo"; constraints = []; };
                    description = "One hundred and thirty six";
                }
                Verbatim "end"
            ];
        }

[<Fact>]
let ``Free-form closing expression bracket is parsed as verbatim`` () =
    assertParsesTo
        "asd }} asd"
        {
            components = [
                Verbatim "asd }} asd"
            ]
        }

[<Fact>]
let ``Empty identifiers are not allowed`` () =
    assertParseError "{{:a:}}"
