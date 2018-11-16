module Tests

open System
open Xunit


open Ast
open FParsec.CharParsers

let parseOrFail template =
    match Parser.parse template with
    | ParserResult.Success (result, state, pos) -> result
    | ParserResult.Failure (message, error, state) -> failwith message

let assertParseError template =
    match Parser.parse template with
    | ParserResult.Success _ -> failwith "Expected parser to fail"
    | ParserResult.Failure _ -> Assert.True(true)

let assertParsesTo template expected =
    Assert.StrictEqual(expected, parseOrFail template)

let assertParsesToFlattened template expected =
    Assert.StrictEqual(
        expected |> flatten,
        template |> parseOrFail |> flatten
        )

[<Fact>]
let ``Empty template yields empty component list`` () =
    assertParsesTo "" []

[<Fact>]
let ``Single expression`` () =
    assertParsesTo
        "{{foo:string:This is the description}}"
        [
            Expr {
                symbol = "foo";
                kind = { name = "string"; hints = []; };
                description = "This is the description";
            }
        ]

[<Fact>]
let ``Verbatim text only`` () =
    assertParsesTo "foo bar baz" [ Text "foo bar baz" ]

[<Fact>]
let ``Expression in middle of text`` () =
    assertParsesTo
        "begin{{fourteen:thirtyTwo:One hundred and thirty six}}end"
        [
            Text "begin"
            Expr {
                symbol = "fourteen";
                kind = { name = "thirtyTwo"; hints = []; };
                description = "One hundred and thirty six";
            }
            Text "end"
        ]

[<Fact>]
let ``Free-form closing expression bracket is parsed as text`` () =
    assertParsesTo "asd }} asd" [ Text "asd }} asd" ]

[<Fact>]
let ``Unclosed expression bracket fails to parse`` () =
    assertParseError "asd {{foo:bar:baz"
    assertParseError "asd {{foo:bar "

[<Fact>]
let ``Empty identifiers are not allowed`` () =
    assertParseError "{{::}}"


[<Fact>]
let ``Two expressions back to back`` () =
    assertParsesTo
        "{{foo:bar:baz}}{{foo:bar:baz}}"
        [
            Expr { symbol = "foo"; kind = { name = "bar"; hints = [] }; description = "baz" }
            Expr { symbol = "foo"; kind = { name = "bar"; hints = [] }; description = "baz" }
        ]

[<Fact>]
let ``Flatten works`` () =
    Assert.StrictEqual(
        [ Text "foobarbaz" ],
        flatten [ Text "foo"; Text "bar"; Text "baz"; ]
    )

[<Fact>]
let ``Escape sequences`` () =
    assertParsesToFlattened "\\{{" [ Text "{{" ]
    assertParsesToFlattened "\\\\" [ Text "\\" ]
    assertParsesToFlattened "\\\\\\"   [ Text "\\\\" ]
    assertParsesToFlattened "\\\\\\\\" [ Text "\\\\" ]
    assertParsesToFlattened "\\\\\\{{" [ Text "\\{{" ]
    assertParseError "\\\\{{"

    assertParsesToFlattened
        "foo\\\\{{foo:bar:baz}}"
        [
            Text "foo\\"
            Expr {
                symbol = "foo"
                kind = { name = "bar"; hints = [] }
                description = "baz"
            }
        ]

    assertParsesToFlattened
        "foo\\{{foo:bar:baz}}" [ Text "foo{{foo:bar:baz}}" ]

[<Fact>]
let ``Spaces around symbol/type/description are ignored`` () =
    assertParsesTo
        "{{\tfoo : bar \t \t : baz \t}}"
        [
            Expr {
                symbol = "foo"
                kind = { name = "bar"; hints = [] }
                description = "baz"
            }
        ]
