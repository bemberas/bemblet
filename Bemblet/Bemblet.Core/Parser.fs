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


let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    

let exprContent = 
    let symbol = identifier .>> (skipChar ':')
    let kind = identifier
    let description = identifier

    pipe2    symbol kind
        (fun symbol kind ->
            {
                symbol = symbol;
                kind = { name = kind; constraints = [] }
                description = "";
            })


let expr = between openExpr closeExpr exprContent 

let verbatim =
    (manyCharsTill anyChar (openExpr <|> eof))


let exprComp = expr |>> Expr
let verbatimComp = verbatim |>> Verbatim

let comp = exprComp <|> verbatimComp

let document =
    (manyTill comp eof) |>> fun x -> { components = x; }

let parse template = run document template