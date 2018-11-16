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