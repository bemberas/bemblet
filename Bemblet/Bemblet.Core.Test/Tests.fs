module Tests

open System
open Xunit


open Ast
open FParsec.CharParsers

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
        "{{foo:string}}"
        {   components = [
                Expr {
                    symbol = "foo";
                    kind = { name = "string"; constraints = []; };
                    description = "";
                }
            ];
        }

[<Fact>]
let ``Verbatim text only`` () =
    assertParsesTo
        "foo bar baz"
        { components = [ Verbatim "foo bar baz" ] }