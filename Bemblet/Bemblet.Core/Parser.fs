module Parser

open FParsec
open FParsec.CharParsers
open FParsec.Primitives
open Ast

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let wsCharArray = [| ' '; '\t' |] 

let wsChar =
    wsCharArray
    |> Seq.map skipChar
    |> choice

let ws = many wsChar

let openExpr = skipString "{{"
let closeExpr = skipString "}}"
let exprFieldSep = skipChar ':'

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

let description =
    let pEnd = lookAhead (closeExpr <|> exprFieldSep)
    ws >>. manyCharsTill anyChar pEnd
      |>> (fun x -> x.TrimEnd(wsCharArray))

let identAndSep = ws >>. identifier .>> ws .>> exprFieldSep

let exprContent =

    let symbol = identAndSep
    let kind   = identAndSep

    pipe3 symbol kind description
     (fun symbol kind description ->
         {
             symbol = symbol;
             kind = { name = kind; hints = [] }
             description = description;
         })

let escape =
    skipChar '\\' >>. choice [
        pstring "\\"
        pstring "{{"
    ]


let expr = between openExpr closeExpr exprContent

let closeText =
    choice [
        openExpr
        eof
        escape >>% ()
    ]
    |> lookAhead

let text = (manyCharsTill anyChar closeText)

let frag =
    choice [
        attempt escape |>> Text
        expr |>> Expr
        text |>> Text
    ]

let document = manyTill frag eof

let parse = run document
