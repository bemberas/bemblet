module Ast

type Hint =
    {
        key: string
        value: string
    }

type Kind =
    {
        name: string
        hints: Hint list
    }

type Expr =
    {
        symbol: string
        kind: Kind
        description: string
    }

type Fragment =
    | Expr of Expr
    | Text of string

type Document = Fragment list

let flatten document =
    List.foldBack
        (fun x xs ->
            match x :: xs with
            | Text t1 :: Text t2 :: xs -> Text (t1 + t2) :: xs
            | x -> x
        )
        document
        []
