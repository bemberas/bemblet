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

let openExpr = skipString "{{"
let closeExpr = skipString "}}"
let exprFieldSep = skipChar ':'


let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    

let description =
    manyCharsTill anyChar (lookAhead (closeExpr <|> exprFieldSep))

let exprContent = 
    let symbol = identifier .>> exprFieldSep
    let kind = identifier .>> exprFieldSep

    pipe3 symbol kind description
     (fun symbol kind description ->
         {
             symbol = symbol;
             kind = { name = kind; constraints = [] }
             description = description;
         })


let expr = between openExpr closeExpr exprContent 

let text =
    (manyCharsTill anyChar (lookAhead (openExpr <|> eof)))

let exprComp = expr |>> Expr
let textComp = text |>> Text

let comp =
    choice [
        exprComp
        textComp
    ]

let document = manyTill comp eof

let parse template = run document template