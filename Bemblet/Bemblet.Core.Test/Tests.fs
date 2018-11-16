module Tests

open System
open Xunit


open Ast

let assertParsesTo template ast =
    Assert.Equal(ast, Parser.parse template)

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