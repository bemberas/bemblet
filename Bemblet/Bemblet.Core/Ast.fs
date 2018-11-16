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